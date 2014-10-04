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

{- Functions for maintaining user list and session state.
-}

module Network.Gitit.Cache ( expireCachedFile
                           , lookupCache
                           , cacheContents )
where

import qualified Data.ByteString as B (ByteString, readFile, writeFile)
import System.FilePath
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import Network.Gitit.State
import Network.Gitit.Types
import Control.Monad
import Control.Monad.Trans (liftIO)

-- | Expire a cached file, identified by its filename in the filestore.
-- Returns () after deleting a file from the cache, fails if no cached file.
expireCachedFile :: String -> GititServerPart ()
expireCachedFile file = do
  cfg <- getConfig
  let target = cacheDir cfg </> file
  when (useCache cfg) $ do
    exists <- liftIO $ doesFileExist target
    if exists
       then liftIO (removeFile target)
       else mzero

lookupCache :: String -> GititServerPart (Maybe B.ByteString)
lookupCache file = do
  cfg <- getConfig
  let target = cacheDir cfg </> file
  if useCache cfg
     then do
       exists <- liftIO $ doesFileExist target
       if exists
          then liftM Just $ liftIO $ B.readFile target
          else return Nothing
     else return Nothing

cacheContents :: String -> B.ByteString -> GititServerPart ()
cacheContents file contents = do
  cfg <- getConfig
  let target = cacheDir cfg </> file
  let targetDir = takeDirectory target
  when (useCache cfg) $ liftIO $ do
    createDirectoryIfMissing True targetDir
    B.writeFile (cacheDir cfg </> file) contents
