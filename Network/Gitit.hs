{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- | Functions for embedding a gitit wiki into a Happstack application.

The following is a minimal standalone wiki program:

> import Network.Gitit
> import Happstack.Server.SimpleHTTP
> 
> main = do
>   conf <- getDefaultConfig
>   createStaticIfMissing conf
>   createTemplateIfMissing conf
>   createRepoIfMissing conf
>   initializeGititState conf
>   simpleHTTP nullConf{port = 5001} $ wikiHandler conf

Here is a more complex example, which serves different wikis
under different paths, and uses a custom authentication scheme:

> import Network.Gitit
> import Control.Monad
> import Text.XHtml hiding (dir)
> import Happstack.Server.SimpleHTTP
> import My.Auth.System (myGetUser, myLoginUser, myLogoutUser)
> 
> type WikiSpec = (String, FileStoreType, PageType)
> 
> wikis = [ ("markdownWiki", Git, Markdown)
>         , ("latexWiki", Darcs, LaTeX) ]
> 
> -- custom authentication
> withUser :: Handler -> Handler
> withUser handler = do
>   user <- myGetUser
>   localRq (setHeader "REMOTE_USER" user) handler
>
> myAuthHandler = msum
>   [ dir "_login" myLoginUser
>   , dir "_logout" myLogoutUser ]
>
> handlerFor :: Config -> WikiSpec -> ServerPart Response
> handlerFor conf (path', fstype, pagetype) = dir path' $
>   wikiHandler conf{ repositoryPath = path'
>                   , repositoryType = fstype
>                   , defaultPageType = pagetype}
>
> indexPage :: ServerPart Response
> indexPage = ok $ toResponse $
>   (p << "Wiki index") +++
>   ulist << map (\(path', _, _) -> li << hotlink path' << path') wikis
> 
> main = do
>   conf <- getDefaultConfig
>   let conf' = conf{authHandler = myAuthHandler}
>   forM wikis $ \(path', fstype, pagetype) -> do
>     let conf'' = conf'{ repositoryPath = path'
>                       , repositoryType = fstype
>                       , defaultPageType = pagetype
>                       }
>     createStaticIfMissing conf''
>     createRepoIfMissing conf''
>   createTemplateIfMissing conf'
>   initializeGititState conf'
>   simpleHTTP nullConf{port = 5001} $
>     (nullDir >> indexPage) `mplus` msum (map (handlerFor conf') wikis)


-}

module Network.Gitit ( initializeGititState
                     , getDefaultConfig
                     , Config(..)
                     , User(..)
                     , Password(..)
                     , FileStoreType(..)
                     , PageType(..)
                     , wikiHandler
                     , readMimeTypesFile
                     , createRepoIfMissing
                     , createTemplateIfMissing
                     , createStaticIfMissing
                     , reloadTemplates
                     , GititServerPart
                     , loginUserForm
                     )
where
import Network.Gitit.Types
import Network.Gitit.Framework
import Network.Gitit.Server
import Network.Gitit.Handlers
import Network.Gitit.Initialize
import Network.Gitit.Config (readMimeTypesFile, getDefaultConfig)
import Network.Gitit.Authentication
import Control.Monad.Reader
import System.FilePath
import Prelude hiding (readFile)
import Codec.Binary.UTF8.String (decodeString)

wikiHandler :: Config -> ServerPart Response
wikiHandler conf = do
  let static = staticDir conf
  let staticHandler = dir "_static" $ withExpiresHeaders $ msum
                         [ dir "css" $ fileServeStrict [] (static </> "css")
                         , dir "js"  $ fileServeStrict [] (static </> "js")
                         , ignoreFilters >>  -- don't compress images, pdfs, etc.
                             fileServeStrict [] static ]
  let handlers = [ debugHandler | debugMode conf] ++ (authHandler conf : wikiHandlers)
  let fs = filestoreFromConfig conf
  let ws = WikiState { wikiConfig = conf, wikiFileStore = fs }
  if compressResponses conf
     then compressedResponseFilter
     else return ""
  staticHandler `mplus` (mapServerPartT (unpackReaderT ws) $ withUser conf $ msum handlers)

wikiHandlers :: [Handler]
wikiHandlers =
  [ -- redirect /wiki -> /wiki/ when gitit is being served at /wiki
    -- so that relative wikilinks on the page will work properly:
    guardBareBase >> getWikiBase >>= \b -> movedPermanently (b ++ "/") (toResponse ())
  , dir "_user"     currentUser
  , dir "_activity" showActivity
  , dir "_go"       goToPage
  , dir "_search"   searchResults
  , dir "_upload"   $ methodOnly GET  >> requireUser uploadForm 
  , dir "_upload"   $ methodOnly POST >> requireUser uploadFile
  , dir "_random"   $ methodOnly GET  >> randomPage
  , dir "_index"    indexPage
  , dir "_feed"     feedHandler
  , dir "_category" $ path $ categoryPage . decodeString
  , dir "_categories" categoryListPage
  , dir "_expire" $ expireCache
  , guardCommand "showraw" >> msum
      [ showRawPage
      , guardPath isSourceCode >> showFileAsText ]
  , guardCommand "history" >> msum
      [ showPageHistory
      , guardPath isSourceCode >> showFileHistory ]
  , guardCommand "edit" >>
      (requireUser $ unlessNoEdit editPage showPage)
  , guardCommand "diff" >> msum
      [ showPageDiff
      , guardPath isSourceCode >> showFileDiff ]
  , guardCommand "export"  >> exportPage
  , guardCommand "cancel"  >> showPage
  , guardCommand "discuss" >> discussPage
  , guardCommand "update"  >> methodOnly POST >>
      requireUser (unlessNoEdit updatePage showPage)
  , guardCommand "delete"  >> msum
      [ methodOnly GET  >>
          requireUser (unlessNoDelete confirmDelete showPage)
      , methodOnly POST >>
          requireUser (unlessNoDelete deletePage showPage) ]
  , guardIndex >> indexPage
  , guardPath isPreview >> preview
  , showPage
  , guardPath isSourceCode >> showHighlightedSource
  , handleAny
  , createPage
  ]

reloadTemplates :: ServerPart Response
reloadTemplates = do
  liftIO $ recompilePageTemplate
  ok $ toResponse "Page templates have been recompiled."

unpackReaderT:: (Monad m)
    => c 
    -> (ReaderT c m) (Maybe ((Either Response a), FilterFun Response))
    -> m (Maybe ((Either Response a), FilterFun Response))
unpackReaderT st handler = runReaderT handler st
