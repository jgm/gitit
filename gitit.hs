{-# LANGUAGE CPP #-}
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

module Main where

import Network.Gitit
import Network.Gitit.Server
import Network.Gitit.Util (readFileUTF8)
import System.Directory
import Data.Maybe (isNothing)
import Control.Monad.Reader
import System.Log.Logger (Priority(..), setLevel, setHandlers,
        getLogger, saveGlobalLogger)
import System.Log.Handler.Simple (fileHandler)
import System.Environment
import System.Exit
import System.IO (stderr)
import System.Console.GetOpt
import Network.Socket hiding (Debug)
import Network.URI
import Data.Version (showVersion)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (fromString)

import Paths_gitit (version, getDataFileName)

main :: IO ()
main = do

  -- parse options to get config file
  opts <- getArgs >>= parseArgs
  defaultConfig <- getDefaultConfig
  conf <- foldM handleFlag defaultConfig opts
  -- check for external programs that are needed
  let repoProg = case repositoryType conf of
                       Mercurial   -> "hg"
                       Darcs       -> "darcs"
                       Git         -> "git"
  let prereqs = ["grep", repoProg]
  forM_ prereqs $ \prog ->
    findExecutable prog >>= \mbFind ->
    when (isNothing mbFind) $ error $
      "Required program '" ++ prog ++ "' not found in system path."

  -- set up logging
  let level = if debugMode conf then DEBUG else logLevel conf
  logFileHandler <- fileHandler (logFile conf) level
  serverLogger <- getLogger "Happstack.Server.AccessLog.Combined"
  gititLogger <- getLogger "gitit"
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] serverLogger
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] gititLogger

  let conf' = conf{logLevel = level}

  -- setup the page repository, template, and static files, if they don't exist
  createRepoIfMissing conf'
  createStaticIfMissing conf'
  createTemplateIfMissing conf'

  -- initialize state
  initializeGititState conf'

  let serverConf = nullConf { validator = Nothing, port = portNumber conf',
                             timeout = 20, logAccess = Nothing }

  -- open the requested interface
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  device <- inet_addr (getListenOrDefault opts)
  bindSocket sock (SockAddrInet (toEnum (portNumber conf')) device)
  listen sock 10

  -- start the server
  simpleHTTPWithSocket sock serverConf $ msum [ wiki conf'
                               , dir "_reloadTemplates" reloadTemplates
                               ]

data Opt
    = Help
    | ConfigFile FilePath
    | Port Int
	| Listen String
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
   , Option ['p'] ["port"] (ReqArg (Port . read) "PORT")
        "Specify port"
   , Option ['l'] ["listen"] (ReqArg (Listen . checkListen) "INTERFACE")
        "Specify IP address to listen on"
   , Option [] ["print-default-config"] (NoArg PrintDefaultConfig)
        "Print default configuration"
   , Option [] ["debug"] (NoArg Debug)
        "Print debugging information on each request"
   , Option ['f'] ["config-file"] (ReqArg ConfigFile "FILE")
        "Specify configuration file"
   ]

checkListen :: String -> String
checkListen l | isIPv6address l = l
              | isIPv4address l = l
              | otherwise       = error "Gitit.checkListen: Not a valid interface name"

getListenOrDefault :: [Opt] -> String
getListenOrDefault [] = "0.0.0.0"
getListenOrDefault ((Listen l):_) = l
getListenOrDefault (_:os) = getListenOrDefault os

parseArgs :: [String] -> IO [Opt]
parseArgs argv = do
  progname <- getProgName
  case getOpt Permute flags argv of
    (opts,_,[])  -> return opts
    (_,_,errs)   -> putErr (ExitFailure 1) (concat errs ++ usageInfo (usageHeader progname) flags)

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
    Help               -> putErr ExitSuccess (usageInfo (usageHeader progname) flags)
    Version            -> putErr ExitSuccess (progname ++ " version " ++ showVersion version ++ compileInfo ++ copyrightMessage)
    PrintDefaultConfig -> getDataFileName "data/default.conf" >>= readFileUTF8 >>= B.putStrLn . fromString >> exitWith ExitSuccess
    Debug              -> return conf{ debugMode = True }
    Port p             -> return conf{ portNumber = p }
    ConfigFile fname   -> getConfigFromFile fname
    Listen _           -> return conf

putErr :: ExitCode -> String -> IO a
putErr c s = B.hPutStrLn stderr (fromString s) >> exitWith c
