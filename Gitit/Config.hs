{-# LANGUAGE CPP, FlexibleContexts #-}
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
import Gitit.State (Config(..), Repository(..), PageType(..))
import System.Environment
import System.Exit
import System.IO (stdout, stderr, hPutStrLn)
import System.Console.GetOpt
import Data.ConfigFile
import Control.Monad.Error
import System.Log.Logger ()
import Data.List (intercalate)
import Data.Char (toLower, toUpper, isDigit)
import Paths_gitit (getDataFileName)

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
   , Option ['p'] ["port"] (ReqArg (Port . read) "PORT")
        "Specify port"
   , Option [] ["print-default-config"] (NoArg PrintDefaultConfig)
        "Print default configuration"
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

forceEither :: Show e => Either e a -> a
forceEither = either (\e -> error (show e)) id

{-
setDefault :: MonadError CPError m => OptionSpec -> String -> ConfigParser -> m ConfigParser
setDefault opt val cp = set cp "DEFAULT" opt val

defaultConfigParser :: ConfigParser
defaultConfigParser = forceEither $
  setDefault "repository-type" "Git" emptyCP >>=
  setDefault "repository-path" "wikidata" >>=
  setDefault "default-page-type" "Markdown" >>=
  setDefault "user-file" "gitit-users" >>=
  setDefault "template-file" "template.html" >>=
  setDefault "log-file" "gitit.log" >>=
  setDefault "log-level" "WARNING" >>=
  setDefault "static-dir" "static" >>=
  setDefault "plugins" "" >>=
  setDefault "table-of-contents" "yes" >>=
  setDefault "max-upload-size" "100000" >>=
  setDefault "port" "5001" >>=
  setDefault "debug-mode" "no" >>=
  setDefault "front-page" "Front Page" >>=
  setDefault "no-edit" "Help" >>=
  setDefault "no-delete" "Front Page, Help" >>=
  setDefault "access-question" "" >>=
  setDefault "access-question-answers" "" >>=
  setDefault "use-recaptcha" "no" >>=
  setDefault "recaptcha-public-key" "" >>=
  setDefault "recaptcha-private-key" "" >>=
  setDefault "max-cache-size" "2000000" >>=
  setDefault "mime-types-file" "/etc/mime.types"
-}

handleFlag :: Config -> Opt -> IO Config
handleFlag conf opt = do
  progname <- getProgName
  case opt of
    Help               -> hPutStrLn stderr (usageInfo (usageHeader progname) flags) >> exitWith ExitSuccess
    Version            -> hPutStrLn stderr (progname ++ " version " ++ _VERSION ++ compileInfo ++ copyrightMessage) >> exitWith ExitSuccess
    PrintDefaultConfig -> getDataFileName "data/default.conf" >>= readFile >>= hPutStrLn stdout >> exitWith ExitSuccess
    Debug              -> return conf{ debugMode = True }
    Port p             -> return conf{ portNumber = p }
    ConfigFile fname   -> do
      defaultCP <- getDataFileName "data/default.conf" >>= readfile emptyCP
      readfile (forceEither defaultCP) fname >>= extractConfig . forceEither

extractConfig :: ConfigParser -> IO Config
extractConfig cp = do
  config <- runErrorT $ do
      cfRepositoryType <- get cp "DEFAULT" "repository-type"
      cfRepositoryPath <- get cp "DEFAULT" "repository-path"
      cfDefaultPageType <- get cp "DEFAULT" "default-page-type"
      cfUserFile <- get cp "DEFAULT" "user-file"
      cfTemplateFile <- get cp "DEFAULT" "template-file"
      cfLogFile <- get cp "DEFAULT" "log-file"
      cfLogLevel <- get cp "DEFAULT" "log-level"
      cfStaticDir <- get cp "DEFAULT" "static-dir"
      cfPlugins <- get cp "DEFAULT" "plugins"
      cfTableOfContents <- get cp "DEFAULT" "table-of-contents"
      cfMaxUploadSize <- get cp "DEFAULT" "max-upload-size"
      cfPort <- get cp "DEFAULT" "port"
      cfDebugMode <- get cp "DEFAULT" "debug-mode"
      cfFrontPage <- get cp "DEFAULT" "front-page"
      cfNoEdit <- get cp "DEFAULT" "no-edit"
      cfNoDelete <- get cp "DEFAULT" "no-delete"
      cfAccessQuestion <- get cp "DEFAULT" "access-question"
      cfAccessQuestionAnswers <- get cp "DEFAULT" "access-question-answers"
      cfUseRecaptcha <- get cp "DEFAULT" "use-recaptcha"
      cfRecaptchaPublicKey <- get cp "DEFAULT" "recaptcha-public-key"
      cfRecaptchaPrivateKey <- get cp "DEFAULT" "recaptcha-private-key"
      cfMaxCacheSize <- get cp "DEFAULT" "max-cache-size"
      cfMimeTypesFile <- get cp "DEFAULT" "mime-types-file"
      return Config{
          repository          = case (map toLower $ cfRepositoryType) of
                                     "git"   -> Git (cfRepositoryPath)
                                     "darcs" -> Darcs (cfRepositoryPath)
                                     x       -> error $ "Unknown repository type: " ++ x
        , defaultPageType     = case (map toLower cfDefaultPageType) of
                                     "markdown"   -> Markdown
                                     "rst"        -> RST
                                     x            -> error $ "Unknown page type: " ++ x
        , userFile            = cfUserFile 
        , templateFile        = cfTemplateFile
        , logFile             = cfLogFile
        , logLevel            = let levelString = map toUpper cfLogLevel
                                    levels = ["DEBUG", "INFO", "NOTICE", "WARNING", "ERROR",
                                              "CRITICAL", "ALERT", "EMERGENCY"]
                                in  if levelString `elem` levels
                                       then read levelString
                                       else error $ "Invalid log-level.\nLegal values are: " ++ (intercalate ", " levels)
        , staticDir           = cfStaticDir
        , pluginModules       = splitCommaList cfPlugins 
        , tableOfContents     = cfTableOfContents 
        , maxUploadSize       = readNumber "max-upload-size" cfMaxUploadSize
        , portNumber          = readNumber "port" cfPort
        , debugMode           = cfDebugMode
        , frontPage           = cfFrontPage
        , noEdit              = splitCommaList cfNoEdit
        , noDelete            = splitCommaList cfNoDelete
        , accessQuestion      = if null cfAccessQuestion
                                   then Nothing
                                   else Just (cfAccessQuestion, splitCommaList cfAccessQuestionAnswers)  
        , useRecaptcha        = cfUseRecaptcha 
        , recaptchaPublicKey  = cfRecaptchaPublicKey 
        , recaptchaPrivateKey = cfRecaptchaPrivateKey
        , maxCacheSize        = readNumber "max-cache-size" cfMaxCacheSize
        , mimeTypesFile       = cfMimeTypesFile }
  case config of
        Left (ParseError e, e') -> error $ "Parse error: " ++ e ++ "\n" ++ e'
        Left e                  -> error (show e)
        Right c                 -> return c

readNumber :: (Read a) => String -> String -> a
readNumber opt x = if all isDigit x
                      then read x
                      else error $ opt ++ " must be a number."

splitCommaList :: String -> [String]
splitCommaList l =
  let (first,rest) = break (== ',') l
      first' = lrStrip first
  in  if null rest
         then if null first' then [] else [first']
         else first' : splitCommaList (tail rest)

lrStrip :: String -> String
lrStrip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

getConfigFromOpts :: IO Config
getConfigFromOpts = do
  defaultConfig <- getDataFileName "data/default.conf" >>= readfile emptyCP >>= extractConfig . forceEither
  getArgs >>= parseArgs >>= foldM handleFlag defaultConfig

