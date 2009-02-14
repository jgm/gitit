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

{- Functions for loading plugins.
-}

module Gitit.Plugins ( loadPlugins )
where
import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce
import Text.Pandoc
import Control.Monad (liftM)
import System.FilePath
import System.Directory (getDirectoryContents)

loadPlugins :: FilePath -> IO [Pandoc -> IO Pandoc]
loadPlugins pluginsDir = do
  pluginPaths <- liftM (filter (\f -> takeExtension f `elem` [".hs",".lhs"])) $ getDirectoryContents pluginsDir 
  mapM loadPlugin $ map (pluginsDir </>) pluginPaths

loadPlugin :: FilePath -> IO (Pandoc -> IO Pandoc)
loadPlugin pluginPath = do
  putStrLn $ "Loading plugin: " ++ pluginPath
  defaultErrorHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget pluginPath Nothing
      addTarget target
      r <- load LoadAllTargets
      case r of
        Failed -> error $ "Error loading plugin: " ++ pluginPath
        Succeeded -> do
          m <- findModule (mkModuleName "GititPlugin") Nothing
          p <- findModule (mkModuleName "Text.Pandoc") Nothing
          pr <- findModule (mkModuleName "Prelude") Nothing
          setContext [] [m, p, pr]
          value <- compileExpr ("(processWithM GititPlugin.transform :: Pandoc -> IO Pandoc)")
          do let value' = (unsafeCoerce value) :: Pandoc -> IO Pandoc
             return value'
