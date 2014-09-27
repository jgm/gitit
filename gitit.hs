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
import Network.Gitit.Compat.Except()
import Control.Monad.Reader
import System.Log.Logger (Priority(..), setLevel, setHandlers,
        getLogger, saveGlobalLogger)
import System.Log.Handler.Simple (fileHandler)
import System.Environment
import System.Exit
import System.IO (stderr)
import System.Console.GetOpt
import Network.Socket hiding (Debug)
import Data.Version (showVersion)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (fromString)

import Paths_gitit (version, getDataFileName)

main :: IO ()
main = do

  -- parse options to get config file
  args <- getArgs >>= parseArgs

  -- sequence in Either monad gets first Left or all Rights
  opts <- case sequence args of
    Left Help -> putErr ExitSuccess =<< usageMessage
    Left Version -> do
        progname <- getProgName
        putErr ExitSuccess (progname ++ " version " ++
            showVersion version ++ compileInfo ++ copyrightMessage)
    Left PrintDefaultConfig -> getDataFileName "data/default.conf" >>=
        readFileUTF8 >>= B.putStrLn . fromString >> exitSuccess
    Right xs -> return xs

  conf' <- case [f | ConfigFile f <- opts] of
                fs -> getConfigFromFiles fs

  let conf = foldl handleFlag conf' opts

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

  -- setup the page repository, template, and static files, if they don't exist
  createRepoIfMissing conf
  createStaticIfMissing conf
  createTemplateIfMissing conf

  -- initialize state
  initializeGititState conf

  let serverConf = nullConf { validator = Nothing, port = portNumber conf,
                             timeout = 20, logAccess = Nothing }

  -- open the requested interface
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  device <- inet_addr (address conf)
  bindSocket sock (SockAddrInet (toEnum (portNumber conf)) device)
  listen sock 10

  -- start the server
  simpleHTTPWithSocket sock serverConf $ msum [ wiki conf
                               , dir "_reloadTemplates" reloadTemplates
                               ]

data ExitOpt
    = Help
    | Version
    | PrintDefaultConfig

data ConfigOpt
    = ConfigFile FilePath
    | Port Int
    | Listen String
    | Debug
    deriving (Eq)

type Opt = Either ExitOpt ConfigOpt

flags :: [OptDescr Opt]
flags =
   [ Option ['h'] ["help"] (NoArg (Left Help))
        "Print this help message"
   , Option ['v'] ["version"] (NoArg (Left Version))
        "Print version information"
   , Option ['p'] ["port"] (ReqArg (Right . Port . read) "PORT")
        "Specify port"
   , Option ['l'] ["listen"] (ReqArg (Right . Listen) "INTERFACE")
        "Specify IP address to listen on"
   , Option [] ["print-default-config"] (NoArg (Left PrintDefaultConfig))
        "Print default configuration"
   , Option [] ["debug"] (NoArg (Right Debug))
        "Print debugging information on each request"
   , Option ['f'] ["config-file"] (ReqArg (Right . ConfigFile) "FILE")
        "Specify configuration file"
   ]

parseArgs :: [String] -> IO [Opt]
parseArgs argv =
  case getOpt Permute flags argv of
    (opts,_,[])  -> return opts
    (_,_,errs)   -> putErr (ExitFailure 1) . (concat errs ++) =<< usageMessage

usageMessage :: IO String
usageMessage = do
  progname <- getProgName
  return $ usageInfo ("Usage:  " ++ progname ++ " [opts...]") flags

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

handleFlag :: Config -> ConfigOpt -> Config
handleFlag conf Debug = conf{ debugMode = True, logLevel = DEBUG }
handleFlag conf (Port p) = conf { portNumber = p }
handleFlag conf (Listen l) = conf { address = l }
handleFlag conf _ = conf

putErr :: ExitCode -> String -> IO a
putErr c s = B.hPutStrLn stderr (fromString s) >> exitWith c
