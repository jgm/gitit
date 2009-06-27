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

{- Utility functions for Gitit.
-}

module Network.Gitit.Util ( withTempDir
                          , orIfNull
                          , splitCategories
                          , trim
                          )
where
import System.Directory (getTemporaryDirectory, createDirectory, removeDirectoryRecursive)
import Control.Exception (bracket)
import System.FilePath ((</>), (<.>))
import System.IO.Error (isAlreadyExistsError)
import Control.Monad.Trans (liftIO)

-- | Perform a function in a temporary directory and clean up.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir baseName = bracket (createTempDir 0 baseName) (removeDirectoryRecursive)

-- | Create a temporary directory with a unique name.
createTempDir :: Integer -> FilePath -> IO FilePath
createTempDir num baseName = do
  sysTempDir <- getTemporaryDirectory
  let dirName = sysTempDir </> baseName <.> show num
  liftIO $ catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
               then createTempDir (num + 1) baseName
               else ioError e

-- | Returns a string, if it is not null, or a backup, if it is.
orIfNull :: String -> String -> String
orIfNull str backup = if null str then backup else str

-- | Split a string containing a list of categories.
splitCategories :: String -> [String]
splitCategories = words . map puncToSpace . trim
     where puncToSpace x | x `elem` ".,;:" = ' '
           puncToSpace x = x

-- | Trim leading and trailing spaces.
trim :: String -> String
trim = reverse . trimLeft . reverse . trimLeft
  where trimLeft = dropWhile (`elem` " \t")
