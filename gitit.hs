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

import Network.Gitit.Plugins ( loadPlugin )
import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.Initialize (createStaticIfMissing, createRepoIfMissing)
import Network.Gitit.Framework
import Network.Gitit.Handlers
import Prelude hiding (writeFile, readFile, catch)
import System.Directory
import System.FilePath ((</>))
import Control.Concurrent
import Network.Gitit.State
import Network.Gitit.Config (getConfigFromOpts)
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
  conf <- getConfigFromOpts

  -- check for external programs that are needed
  let prereqs = ["grep", repositoryType conf]
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
  let level = if debugMode conf then DEBUG else logLevel conf
  logFileHandler <- fileHandler (logFile conf) level
  serverLogger <- getLogger "Happstack.Server"
  -- TODO changes to "Happstack.Server.AccessLog.Combined" for 0.3
  gititLogger <- getLogger "gitit"
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] serverLogger
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] gititLogger

  jsMathExists <- liftIO $ doesFileExist $
                  staticDir conf </> "js" </> "jsMath" </> "easy" </> "load.js"
  logM "gitit" NOTICE $
    if jsMathExists
       then "Found jsMath scripts -- using jsMath"
       else "Did not find jsMath scripts -- not using jsMath"

  -- initialize state
  initializeAppState conf{jsMath = jsMathExists, logLevel = level} users'

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

  let handlers = [ debugHandler | debugMode conf] ++
                 case authenticationMethod conf of
                    FormAuth -> authHandler : wikiHandlers
                    _        -> wikiHandlers
  -- TODO - rearrange so handleAny doesn't get compressed
  let wikiHandler = mapServerPartT (unpackReaderT conf) $
                    if compressResponses conf
                       then compressedResponseFilter >> msum handlers
                       else msum handlers

  -- start the server
  tid <- forkIO $ simpleHTTP serverConf $ msum $
         [staticHandler, wikiHandler]
  waitForTermination

  -- shut down the server
  killThread tid

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

