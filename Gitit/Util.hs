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

module Gitit.Util ( withTempDir
                  , orIfNull
                  , consolidateHeads
                  )
where
import System.Directory (getTemporaryDirectory, createDirectory, removeDirectoryRecursive)
import Control.Exception (bracket)
import System.FilePath ((</>), (<.>))
import System.IO.Error (isAlreadyExistsError)
import Control.Monad.Trans (liftIO)
import Data.List (nub)

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

-- | Map a list of nonempty lists onto a list of pairs of list heads and list of tails.
-- e.g. [[1,2],[1],[2,1]] -> [(1,[[2],[]]), (2,[[1]])]
consolidateHeads :: Eq a => [[a]] -> [(a,[[a]])]
consolidateHeads lst =
  let heads = nub $ map head lst
      tailsFor h = map tail [l | l <- lst, head l == h]
  in  map (\h -> (h, tailsFor h)) heads


