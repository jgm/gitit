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

{- Functions for parsing command line options and reading the config file. 
-}

module Gitit.Config ( getConfigFromOpts )
where
import Gitit.State (Config(..), defaultConfig)
import System.Environment
import System.Exit
import System.IO (stderr, hPutStrLn)
import System.Console.GetOpt
import Control.Monad (liftM, foldM)

data Opt
    = Help
    | ConfigFile FilePath
    | Port Int
    | Debug
    | Version
    | PrintDefaultConfig
    deriving (Eq)

flags :: [OptDescr Opt]
flags =
   [ Option ['h'] ["help"] (NoArg Help)
        "Print this help message"
   , Option ['v'] ["version"] (NoArg Version)
        "Print version information"
   , Option [] ["print-default-config"] (NoArg PrintDefaultConfig)
        "Print default configuration"
   , Option ['p'] ["port"] (ReqArg (Port . read) "PORT")
        "Specify port"
   , Option ['d'] ["debug"] (NoArg Debug)
        "Print debugging information on each request"
   , Option ['f'] ["config-file"] (ReqArg ConfigFile "FILE")
        "Specify configuration file"
   ]

parseArgs :: [String] -> IO [Opt]
parseArgs argv = do
  progname <- getProgName
  case getOpt Permute flags argv of
    (opts,_,[])  -> return opts
    (_,_,errs)   -> hPutStrLn stderr (concat errs ++ usageInfo (usageHeader progname) flags) >>
                       exitWith (ExitFailure 1)

usageHeader :: String -> String
usageHeader progname = "Usage:  " ++ progname ++ " [opts...]"

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2008 John MacFarlane\n" ++
                   "This is free software; see the source for copying conditions.  There is no\n" ++
                   "warranty, not even for merchantability or fitness for a particular purpose."

compileInfo :: String
compileInfo =
#ifdef _PLUGINS
  " +plugins"
#else
  " -plugins"
#endif

handleFlag :: Config -> Opt -> IO Config
handleFlag conf opt = do
  progname <- getProgName
  case opt of
    Help               -> hPutStrLn stderr (usageInfo (usageHeader progname) flags) >> exitWith ExitSuccess
    Version            -> hPutStrLn stderr (progname ++ " version " ++ _VERSION ++ compileInfo ++ copyrightMessage) >> exitWith ExitSuccess
    PrintDefaultConfig -> print conf >> exitWith ExitSuccess
    Debug              -> return $ conf { debugMode = True }
    Port p             -> return $ conf { portNumber = p }
    ConfigFile f       -> liftM read (readFile f)

getConfigFromOpts :: IO Config
getConfigFromOpts = getArgs >>= parseArgs >>= foldM handleFlag defaultConfig
