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

{- Mapping from file extensions to mime types. 
-}

module Gitit.MimeTypes ( readMimeTypesFile )
where
import qualified Data.Map as M
import qualified HAppS.Server
import System.IO (stderr, hPutStrLn)

-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String String)
readMimeTypesFile f = catch (readFile f >>= return . foldr go M.empty . map words . lines) $
                            handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (\ext m' -> M.insert ext x m') m xs
           handleMimeTypesFileNotFound e = do
             hPutStrLn stderr $ "Could not read mime types file: " ++ f
             hPutStrLn stderr $ show e
             hPutStrLn stderr $ "Using defaults instead."
             return HAppS.Server.mimeTypes

