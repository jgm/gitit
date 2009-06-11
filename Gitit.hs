{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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

module Main where

import Gitit.Plugins ( loadPlugin )
import Gitit.Types
import Gitit.Server
import Gitit.Initialize (createStaticIfMissing, createRepoIfMissing)
import Gitit.Framework
import Gitit.Handlers
import Prelude hiding (writeFile, readFile, catch)
import System.Directory
import Control.Concurrent
import Gitit.State
import Gitit.Config (getConfigFromOpts)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe (isNothing)
import qualified Data.Map as M
import System.IO.UTF8 (readFile)
import Control.Monad.Reader
import System.Log.Logger (logM, Priority(..), setLevel, setHandlers,
        getLogger, saveGlobalLogger)
import System.Log.Handler.Simple (fileHandler)

main :: IO ()
main = do

  -- parse options to get config file
  conf' <- getConfigFromOpts
  let conf = if debugMode conf' then conf'{logLevel = DEBUG} else conf'

  -- check for external programs that are needed
  let prereqs = "grep" : case repository conf of
                      Git _        -> ["git"]
                      Darcs _      -> ["darcs"]
  forM_ prereqs $ \prog ->
    findExecutable prog >>= \mbFind ->
    when (isNothing mbFind) $ error $
      "Required program '" ++ prog ++ "' not found in system path."

  -- read user file and update state
  userFileExists <- doesFileExist $ userFile conf
  users' <- if userFileExists
               then liftM (M.fromList . read) $ readFile $ userFile conf
               else return M.empty

  -- set up logging
  let level = logLevel conf
  logFileHandler <- fileHandler (logFile conf) level
  serverLogger <- getLogger "Happstack.Server"
  -- TODO changes to "Happstack.Server.AccessLog.Combined" for 0.3
  gititLogger <- getLogger "gitit"
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] serverLogger
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] gititLogger

  -- log config file in DEBUG
  logM "gitit" DEBUG (show conf)

  -- initialize state
  initializeAppState conf users'

  -- setup the page repository and static files, if they don't exist
  createRepoIfMissing conf
  let staticdir = staticDir conf
  createStaticIfMissing staticdir

  -- load plugins
  let loadPluginAndLog plg = logM "gitit" WARNING ("Loading plugin '" ++ plg ++ "'...") >> loadPlugin plg
  plugins' <- mapM loadPluginAndLog (pluginModules conf)
  updateAppState $ \s -> s{ plugins = plugins' }
  unless (null $ pluginModules conf) $ logM "gitit" WARNING "Finished loading plugins."

  let serverConf = Conf { validator = Nothing, port = portNumber conf }
  let staticHandler = dir "_static" $
                      withExpiresHeaders $ fileServe [] staticdir

  let handlers = case authenticationMethod conf of
                    FormAuth -> authHandler : wikiHandlers
                    _        -> wikiHandlers

  -- start the server
  tid <- forkIO $ simpleHTTP serverConf $ msum $
         staticHandler : ([ debugHandler | debugMode conf ] ++ handlers)
  waitForTermination

  -- shut down the server
  killThread tid

wikiHandlers :: [Handler]
wikiHandlers =
  [ handlePath "_activity"  GET  showActivity
  , handlePath "_go"        POST goToPage
  , handlePath "_search"    POST searchResults
  , handlePath "_search"    GET  searchResults
  , handlePath "_upload"    GET  (ifLoggedIn uploadForm loginUserForm)
  , handlePath "_upload"    POST (ifLoggedIn uploadFile loginUserForm)
  , handlePath "_random"    GET  randomPage
  , handle isIndex          GET  indexPage
  , handle isPreview        POST preview
  , withCommand "showraw" [ handlePage GET showRawPage
                          , handle isSourceCode GET showFileAsText ]
  , withCommand "history" [ handlePage GET showPageHistory
                          , handle isSourceCode GET showFileHistory ]
  , withCommand "edit"    [ handlePage GET $ unlessNoEdit
                              (ifLoggedIn editPage loginUserForm) showPage ]
  , withCommand "diff"    [ handlePage GET showPageDiff
                          , handle isSourceCode GET showFileDiff ]
  , withCommand "export"  [ handlePage POST exportPage
                          , handlePage GET exportPage ]
  , withCommand "cancel"  [ handlePage POST showPage ]
  , withCommand "discuss" [ handlePage GET discussPage ]
  , withCommand "update"  [ handlePage POST $ unlessNoEdit
                              (ifLoggedIn updatePage loginUserForm) showPage ]
  , withCommand "delete"  [ handlePage GET  $ unlessNoDelete
                              (ifLoggedIn confirmDelete loginUserForm) showPage,
                            handlePage POST $ unlessNoDelete
                              (ifLoggedIn deletePage loginUserForm) showPage ]
  , handlePage GET showPage
  , handle isSourceCode GET showHighlightedSource
  , handleAny
  , handlePage GET  createPage
  , handlePage POST createPage  -- if they click Discard on a new page
  ]

isIndex :: String -> Bool
isIndex ""       = False
isIndex "_index" = True
isIndex x        = "_index/" `isPrefixOf` x || last x == '/'

isPreview :: String -> Bool
isPreview x  = "___preview" `isSuffixOf` x
-- We choose something that is unlikely to occur naturally as a suffix.
-- Why suffix and not prefix?  Because the link is added by a script,
-- and mod_proxy_html doesn't rewrite links in scripts.  So this is
-- to make it possible to use gitit with an alterantive docroot.


