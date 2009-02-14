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

{- Functions for initializing a Gitit wiki.
-}

module Gitit.Initialize ( createStaticIfMissing, createRepoIfMissing )
where
import System.FilePath ((</>), (<.>), takeExtension)
import Data.FileStore
import Gitit.State
import Paths_gitit (getDataFileName)
import qualified Data.ByteString.Lazy as B
import Control.Exception (throwIO, try)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless, forM_, liftM)

-- | Create page repository unless it exists.
createRepoIfMissing :: Config -> IO ()
createRepoIfMissing conf = do
  fs <- getFileStore
  repoExists <- try (initialize fs) >>= \res ->
    case res of
         Right _               -> return False
         Left RepositoryExists -> return True
         Left e                -> throwIO e >> return False
  unless repoExists $ do
    welcomepath <- getDataFileName $ "data" </> "FrontPage.page"
    welcomecontents <- B.readFile welcomepath
    helppath <- getDataFileName $ "data" </> "Help.page"
    helpcontents <- B.readFile helppath
    -- add front page and help page
    create fs (frontPage conf <.> "page") (Author "Gitit" "") "Default front page" welcomecontents
    create fs "Help.page" (Author "Gitit" "") "Default front page" helpcontents
    hPutStrLn stderr "Created repository"

-- | Create static directory unless it exists.
createStaticIfMissing :: FilePath -> IO ()
createStaticIfMissing staticdir = do
  staticExists <- doesDirectoryExist staticdir
  unless staticExists $ do

    let cssdir = staticdir </> "css"
    createDirectoryIfMissing True cssdir
    cssDataDir <- getDataFileName "css"
    cssFiles <- liftM (filter (\f -> takeExtension f == ".css")) $ getDirectoryContents cssDataDir
    forM_ cssFiles $ \f -> copyFile (cssDataDir </> f) (cssdir </> f)

    let icondir = staticdir </> "img" </> "icons" 
    createDirectoryIfMissing True icondir
    iconDataDir <- getDataFileName ("img" </> "icons")
    iconFiles <- liftM (filter (\f -> takeExtension f == ".png")) $ getDirectoryContents iconDataDir
    forM_ iconFiles $ \f -> copyFile (iconDataDir </> f) (icondir </> f)

    logopath <- getDataFileName $ "img" </> "gitit-dog.png"
    copyFile logopath $ staticdir </> "img" </> "logo.png"

    let jsdir = staticdir </> "js"
    createDirectoryIfMissing True jsdir
    let javascripts = ["jquery.min.js", "jquery-ui.packed.js",
                       "folding.js", "dragdiff.js", "preview.js", "search.js", "uploadForm.js"]
    jsDataDir <- getDataFileName "js"
    forM_ javascripts $ \f -> copyFile (jsDataDir </> f) (jsdir </> f)

    hPutStrLn stderr $ "Created " ++ staticdir ++ " directory"

