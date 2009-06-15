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

module Network.Gitit.Initialize ( createStaticIfMissing, createRepoIfMissing )
where
import System.FilePath ((</>), (<.>), takeExtension)
import Data.FileStore
import Network.Gitit.Types
import Paths_gitit (getDataFileName)
import Control.Exception (throwIO, try)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import Control.Monad (unless, forM_, liftM)
import Prelude hiding (readFile)
import System.IO.UTF8
import System.IO (stderr)
import Text.Pandoc
import Text.Pandoc.Shared (HTMLMathMethod(..))

-- | Create page repository unless it exists.
createRepoIfMissing :: Config -> IO ()
createRepoIfMissing conf = do
  let fs = filestore conf
  repoExists <- try (initialize fs) >>= \res ->
    case res of
         Right _               -> return False
         Left RepositoryExists -> return True
         Left e                -> throwIO e >> return False
  let pt = defaultPageType conf
  let toPandoc = readMarkdown
                 defaultParserState{ stateSanitizeHTML = True
                                   , stateSmart = True }
  let defOpts = defaultWriterOptions{
                        writerStandalone = False
                      , writerHTMLMathMethod = JsMath
                               (Just "/_static/js/jsMath/easy/load.js")
                      , writerLiterateHaskell = showLHSBirdTracks conf
                      }
  -- note: we convert this (markdown) to the default page format
  let converter = case defaultPageType conf of
                     Markdown -> id
                     LaTeX    -> writeLaTeX defOpts . toPandoc
                     HTML     -> writeHtmlString defOpts . toPandoc
                     RST      -> writeRST defOpts . toPandoc
  unless repoExists $ do
    welcomepath <- getDataFileName $ "data" </> "FrontPage" <.> "page"
    welcomecontents <- liftM converter $ readFile welcomepath
    helppath <- getDataFileName $ "data" </> "Help" <.> "page"
    helpcontentsInitial <- liftM converter $ readFile helppath
    markuppath <- getDataFileName $ "data" </> "markup" <.> show pt
    helpcontentsMarkup <- liftM converter $ readFile markuppath
    let helpcontents = helpcontentsInitial ++ "\n\n" ++ helpcontentsMarkup
    -- add front page and help page
    create fs (frontPage conf <.> "page") (Author "Gitit" "") "Default front page" welcomecontents
    create fs "Help.page" (Author "Gitit" "") "Default help page" helpcontents
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
                       "dragdiff.js", "preview.js", "search.js", "uploadForm.js"]
    jsDataDir <- getDataFileName "js"
    forM_ javascripts $ \f -> copyFile (jsDataDir </> f) (jsdir </> f)

    hPutStrLn stderr $ "Created " ++ staticdir ++ " directory"

