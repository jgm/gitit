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
>   initializeGititState (userFile conf) (pluginModules conf)
>   simpleHTTP nullConf $ wikiHandler conf

-}

module Network.Gitit ( initializeGititState
                     , getDefaultConfig
                     , Config(..)
                     , wikiHandler
                     , readMimeTypesFile
                     , createRepoIfMissing
                     , createTemplateIfMissing
                     , createStaticIfMissing
                     )
where
import Network.Gitit.Types
import Network.Gitit.Framework
import Network.Gitit.Server
import Network.Gitit.Handlers
import Network.Gitit.Initialize
import Network.Gitit.Config (readMimeTypesFile, getDefaultConfig)
import Control.Monad.Reader
import System.Directory
import System.FilePath
import Prelude hiding (readFile)
import System.IO.UTF8
import Paths_gitit
import qualified Text.StringTemplate as T

wikiHandler :: Config -> ServerPart Response
wikiHandler conf = do
  let staticHandler = dir "_static" $
                      withExpiresHeaders $ fileServe [] $ staticDir conf
  let handlers = [ debugHandler | debugMode conf] ++
                 case authenticationMethod conf of
                     FormAuth -> authHandler : wikiHandlers
                     _        -> wikiHandlers
  let fs = filestoreFromConfig conf
  templateText <- liftIO $ do
    templateExists <- doesFileExist $ templateFile conf
    if templateExists
       then readFile $ templateFile conf
       else getDataFileName ("data" </> "template.html") >>= readFile
  let templ = T.newSTMP templateText
  let ws = WikiState { wikiConfig = conf, wikiFileStore = fs, wikiTemplate = templ }
  -- TODO - rearrange so handleAny doesn't get compressed
  staticHandler `mplus` (mapServerPartT (unpackReaderT ws) $
                        if compressResponses conf
                           then compressedResponseFilter >> msum handlers
                           else msum handlers)

wikiHandlers :: [Handler]
wikiHandlers =
  [ dir "_activity" showActivity
  , dir "_go"       goToPage
  , dir "_search"   searchResults
  , dir "_upload"   $ methodOnly GET  >> ifLoggedIn uploadForm loginUserForm
  , dir "_upload"   $ methodOnly POST >> ifLoggedIn uploadFile loginUserForm
  , dir "_random"   $ methodOnly GET  >> randomPage
  , dir "_index"    indexPage
  , guardCommand "showraw" >> msum
      [ showRawPage
      , guardPath isSourceCode >> showFileAsText ]
  , guardCommand "history" >> msum
      [ showPageHistory
      , guardPath isSourceCode >> showFileHistory ]
  , guardCommand "edit" >>
      (unlessNoEdit (ifLoggedIn editPage loginUserForm) showPage)
  , guardCommand "diff" >> msum
      [ showPageDiff
      , guardPath isSourceCode >> showFileDiff ]
  , guardCommand "export"  >> exportPage
  , guardCommand "cancel"  >> showPage
  , guardCommand "discuss" >> discussPage
  , guardCommand "update"  >> methodOnly POST >>
      unlessNoEdit (ifLoggedIn updatePage loginUserForm) showPage
  , guardCommand "delete"  >> msum
      [ methodOnly GET  >>
          unlessNoDelete (ifLoggedIn confirmDelete loginUserForm) showPage
      , methodOnly POST >>
          unlessNoDelete (ifLoggedIn deletePage loginUserForm) showPage ]
  , guardIndex >> indexPage
  , guardPath isPreview >> preview
  , showPage
  , guardPath isSourceCode >> showHighlightedSource
  , handleAny
  , createPage
  ]

unpackReaderT:: (Monad m)
    => c 
    -> (ReaderT c m) (Maybe ((Either Response a), FilterFun Response))
    -> m (Maybe ((Either Response a), FilterFun Response))
unpackReaderT st handler = runReaderT handler st
