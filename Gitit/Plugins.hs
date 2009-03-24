{-# LANGUAGE CPP #-}
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

module Gitit.Plugins ( loadPlugin )
where
import Gitit.State
import System.FilePath
#ifdef _PLUGINS
import Control.Monad (when)
import Data.List (isInfixOf, isPrefixOf)
import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  plugin <- defaultCleanupHandler defaultDynFlags $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      when (not $ "Gitit.Plugin." `isPrefixOf` pluginName)
        $ do
            addTarget =<< guessTarget pluginName Nothing
            r <- load LoadAllTargets
            case r of
              Failed -> error $ "Error loading plugin: " ++ pluginName
              Succeeded -> return ()
      let modName =
            if "Gitit.Plugin" `isPrefixOf` pluginName
            then pluginName
            else
              (if "Gitit/Plugin/" `isInfixOf` pluginName then ("Gitit.Plugin." ++) else id)
              $ takeBaseName pluginName
      pr <- findModule (mkModuleName "Prelude") Nothing
      i <- findModule (mkModuleName "Gitit.Interface") Nothing
      m <- findModule (mkModuleName modName) Nothing
      setContext [] [m, i, pr]
      value <- compileExpr (modName ++ ".plugin :: Plugin")
      let value' = (unsafeCoerce value) :: Plugin
      return value'
  return plugin

#else

loadPlugin :: FilePath -> IO Plugin
loadPlugin pluginName = do
  error $ "Cannot load plugin '" ++ pluginName ++ "'. gitit was not compiled with plugin support."
  return undefined

#endif
