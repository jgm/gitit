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

{- Auxiliary functions for running shell commands.

   Note:  UTF-8 locale is assumed.
-}

module Gitit.Shell where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO, MonadIO)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (ExitCode)
import System.IO (openTempFile)
import System.Process (runProcess, waitForProcess)
import Codec.Binary.UTF8.String (encodeString)
import HAppS.State (query)
import Gitit.State (repositoryPath, GetConfig(..))

-- | Run shell command and return error status, standard output, and error output.
runShellCommand :: FilePath -> Maybe [(String, String)] -> String -> [String] -> IO (ExitCode, String, String)
runShellCommand workingDir environment command optionList = do
  tempPath <- getTemporaryDirectory
  (outputPath, hOut) <- openTempFile tempPath "out"
  (errorPath, hErr) <- openTempFile tempPath "err"
  hProcess <- runProcess command optionList (Just workingDir) environment Nothing (Just hOut) (Just hErr)
  status <- waitForProcess hProcess
  errorOutput <- readFile errorPath
  output <- readFile outputPath
  removeFile errorPath
  removeFile outputPath
  return (status, errorOutput, output)

runProgCommand :: (MonadIO m) => String -> String -> [String] -> m (ExitCode, String, String)
runProgCommand prog command args = do
  repo <- liftM repositoryPath (query GetConfig)
  liftIO $ runShellCommand repo Nothing prog (command : map encodeString args)
