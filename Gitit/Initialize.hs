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

module Gitit.Initialize ( initializeWiki )
where
import System.FilePath ((</>), (<.>))
import Data.FileStore
import Gitit.State
import Paths_gitit (getDataFileName)
import qualified Data.ByteString.Lazy as B
import Prelude hiding (catch)
import Control.Exception (throwIO, catch)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist)
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless, zipWithM_)

-- | Create repository and public directories, unless they already exist.
initializeWiki :: Config -> IO ()
initializeWiki conf = do
  let staticdir = staticDir conf
  fs <- getFileStore
  repoExists <- catch (initialize fs >> return False)
                      (\e -> if e == RepositoryExists then return True else throwIO e >> return False)
  unless repoExists $ do
    welcomepath <- getDataFileName $ "data" </> "FrontPage.page"
    welcomecontents <- B.readFile welcomepath
    helppath <- getDataFileName $ "data" </> "Help.page"
    helpcontents <- B.readFile helppath
    -- add front page and help page
    create fs (frontPage conf <.> "page") (Author "Gitit" "") "Default front page" welcomecontents
    create fs "Help.page" (Author "Gitit" "") "Default front page" helpcontents
    hPutStrLn stderr "Created repository"
  staticExists <- doesDirectoryExist staticdir
  unless staticExists $ do
    createDirectoryIfMissing True $ staticdir </> "css"
    let stylesheets = map ("css" </>) ["screen.css", "print.css", "ie.css", "hk-pyg.css"]
    stylesheetpaths <- mapM getDataFileName stylesheets
    zipWithM_ copyFile stylesheetpaths (map (staticdir </>) stylesheets)
    createDirectoryIfMissing True $ staticdir </> "img" </> "icons"
    let imgs = map (("img" </>) . ("icons" </>))
                ["cross.png", "doc.png", "email.png", "external.png", "feed.png", "folder.png",
                 "im.png", "key.png", "page.png", "pdf.png", "tick.png", "xls.png"]
    imgpaths <- mapM getDataFileName imgs
    zipWithM_ copyFile imgpaths (map (staticdir </>) imgs)
    logopath <- getDataFileName $ "img" </> "gitit-dog.png"
    copyFile logopath (staticdir </> "img" </> "logo.png")
    createDirectoryIfMissing True $ staticdir </> "js"
    let javascripts = ["jquery.min.js", "jquery-ui.packed.js",
                       "folding.js", "dragdiff.js", "preview.js", "search.js", "uploadForm.js"]
    javascriptpaths <- mapM getDataFileName $ map ("js" </>) javascripts
    zipWithM_ copyFile javascriptpaths $ map ((staticdir </> "js") </>) javascripts
    hPutStrLn stderr $ "Created " ++ staticdir ++ " directory"
  jsMathExists <- doesDirectoryExist (staticdir </> "js" </> "jsMath")
  updateAppState $ \s -> s{ jsMath = jsMathExists }
  unless jsMathExists $ do
    hPutStrLn stderr $ replicate 80 '*' ++
                       "\nWarning:  jsMath not found.\n" ++
                       "If you want support for math, copy the jsMath directory into " ++ staticdir ++ "/js/\n" ++
                       "jsMath can be obtained from http://www.math.union.edu/~dpvc/jsMath/\n" ++
                       replicate 80 '*'


