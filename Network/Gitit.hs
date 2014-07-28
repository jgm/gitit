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
>   simpleHTTP nullConf{port = 5001} $ wiki conf

Here is a more complex example, which serves different wikis
under different paths, and uses a custom authentication scheme:

> import Network.Gitit
> import Control.Monad
> import Text.XHtml hiding (dir)
> import Happstack.Server.SimpleHTTP
>
> type WikiSpec = (String, FileStoreType, PageType)
>
> wikis = [ ("markdownWiki", Git, Markdown)
>         , ("latexWiki", Darcs, LaTeX) ]
>
> -- custom authentication
> myWithUser :: Handler -> Handler
> myWithUser handler = do
>   -- replace the following with a function that retrieves
>   -- the logged in user for your happstack app:
>   user <- return "testuser"
>   localRq (setHeader "REMOTE_USER" user) handler
>
> myAuthHandler = msum
>   [ dir "_login"  $ seeOther "/your/login/url"  $ toResponse ()
>   , dir "_logout" $ seeOther "/your/logout/url" $ toResponse () ]
>
> handlerFor :: Config -> WikiSpec -> ServerPart Response
> handlerFor conf (path', fstype, pagetype) = dir path' $
>   wiki conf{ repositoryPath = path'
>            , repositoryType = fstype
>            , defaultPageType = pagetype}
>
> indexPage :: ServerPart Response
> indexPage = ok $ toResponse $
>   (p << "Wiki index") +++
>   ulist << map (\(path', _, _) -> li << hotlink (path' ++ "/") << path') wikis
>
> main = do
>   conf <- getDefaultConfig
>   let conf' = conf{authHandler = myAuthHandler, withUser = myWithUser}
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

module Network.Gitit (
                     -- * Wiki handlers
                       wiki
                     , reloadTemplates
                     , runHandler
                     -- * Initialization
                     , module Network.Gitit.Initialize
                     -- * Configuration
                     , module Network.Gitit.Config
                     , loginUserForm
                     -- * Types
                     , module Network.Gitit.Types
                     -- * Tools for building handlers
                     , module Network.Gitit.Framework
                     , module Network.Gitit.Layout
                     , module Network.Gitit.ContentTransformer
                     , module Network.Gitit.Page
                     , getFileStore
                     , getUser
                     , getConfig
                     , queryGititState
                     , updateGititState
                     )
where
import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.Handlers
import Network.Gitit.Initialize
import Network.Gitit.Config
import Network.Gitit.Layout
import Network.Gitit.State
        (getFileStore, getUser, getConfig, queryGititState, updateGititState)
import Network.Gitit.ContentTransformer
import Network.Gitit.Page
import Network.Gitit.Authentication (loginUserForm)
import Paths_gitit (getDataFileName)
import Control.Monad.Reader
import Prelude hiding (readFile)
import qualified Data.ByteString.Char8 as B
import System.FilePath ((</>))
import System.Directory (getTemporaryDirectory)
import Safe

-- | Happstack handler for a gitit wiki.
wiki :: Config -> ServerPart Response
wiki conf = do
  tempDir <- liftIO getTemporaryDirectory
  let maxSize = fromIntegral $ maxUploadSize conf
  decodeBody $ defaultBodyPolicy tempDir maxSize maxSize maxSize
  let static = staticDir conf
  defaultStatic <- liftIO $ getDataFileName $ "data" </> "static"
  -- if file not found in staticDir, we check also in the data/static
  -- directory, which contains defaults
  let staticHandler = withExpiresHeaders $
        serveDirectory' static `mplus` serveDirectory' defaultStatic
  let debugHandler' = msum [debugHandler | debugMode conf]
  let handlers = debugHandler' `mplus` authHandler conf `mplus`
                 authenticate ForRead (msum wikiHandlers)
  let fs = filestoreFromConfig conf
  let ws = WikiState { wikiConfig = conf, wikiFileStore = fs }
  if compressResponses conf
     then compressedResponseFilter
     else return ""
  staticHandler `mplus` runHandler ws (withUser conf handlers)

-- | Like 'serveDirectory', but if file is not found, fail instead of
-- returning a 404 error.
serveDirectory' :: FilePath -> ServerPart Response
serveDirectory' p = do
  rq <- askRq
  resp' <- serveDirectory EnableBrowsing [] p
  if rsCode resp' == 404 || lastNote "fileServeStrict'" (rqUri rq) == '/'
     then mzero  -- pass through if not found or directory index
     else
       -- turn off compresion filter unless it's text
       case getHeader "Content-Type" resp' of
            Just ct | B.pack "text/" `B.isPrefixOf` ct -> return resp'
            _ -> ignoreFilters >> return resp'

wikiHandlers :: [Handler]
wikiHandlers =
  [ -- redirect /wiki -> /wiki/ when gitit is being served at /wiki
    -- so that relative wikilinks on the page will work properly:
    guardBareBase >> getWikiBase >>= \b -> movedPermanently (b ++ "/") (toResponse ())
  , dir "_activity" showActivity
  , dir "_go"       goToPage
  , method GET >> dir "_search"   searchResults
  , dir "_upload"   $  do guard =<< return . uploadsAllowed =<< getConfig
                          msum [ method GET  >> authenticate ForModify uploadForm
                                 , method POST >> authenticate ForModify uploadFile ]
  , dir "_random"   $ method GET  >> randomPage
  , dir "_index"    indexPage
  , dir "_feed"     feedHandler
  , dir "_category" categoryPage
  , dir "_categories" categoryListPage
  , dir "_expire"     expireCache
  , dir "_showraw"  $ msum
      [ showRawPage
      , guardPath isSourceCode >> showFileAsText ]
  , dir "_history"  $ msum
      [ showPageHistory
      , guardPath isSourceCode >> showFileHistory ]
  , dir "_edit" $ authenticate ForModify (unlessNoEdit editPage showPage)
  , dir "_diff" $ msum
      [ showPageDiff
      , guardPath isSourceCode >> showFileDiff ]
  , dir "_discuss" discussPage
  , dir "_delete" $ msum
      [ method GET  >>
          authenticate ForModify (unlessNoDelete confirmDelete showPage)
      , method POST >>
          authenticate ForModify (unlessNoDelete deletePage showPage) ]
  , dir "_preview" preview
  , guardIndex >> indexPage
  , guardCommand "export" >> exportPage
  , method POST >> guardCommand "cancel" >> showPage
  , method POST >> guardCommand "update" >>
      authenticate ForModify (unlessNoEdit updatePage showPage)
  , showPage
  , guardPath isSourceCode >> method GET >> showHighlightedSource
  , handleAny
  , notFound =<< (guardPath isPage >> createPage)
  ]

-- | Recompiles the gitit templates.
reloadTemplates :: ServerPart Response
reloadTemplates = do
  liftIO recompilePageTemplate
  ok $ toResponse "Page templates have been recompiled."

-- | Converts a gitit Handler into a standard happstack ServerPart.
runHandler :: WikiState -> Handler -> ServerPart Response
runHandler = mapServerPartT . unpackReaderT

unpackReaderT :: s -> UnWebT (ReaderT s IO) a -> UnWebT IO a
unpackReaderT st uw = runReaderT uw st

