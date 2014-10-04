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

{- | Functions for parsing command line options and reading the config file.
-}

module Network.Gitit.Config ( getConfigFromFile
                            , getDefaultConfig
                            , readMimeTypesFile )
where
import Network.Gitit.Types
import Network.Gitit.Server (mimeTypes)
import Network.Gitit.Framework
import Network.Gitit.Authentication (formAuthHandlers, httpAuthHandlers)
import Network.Gitit.Util (parsePageType, readFileUTF8)
import System.Log.Logger (logM, Priority(..))
import qualified Data.Map as M
import Data.ConfigFile hiding (readfile)
import Control.Monad.Error
import System.Log.Logger ()
import Data.List (intercalate)
import Data.Char (toLower, toUpper, isDigit)
import Paths_gitit (getDataFileName)
import System.FilePath ((</>))
import Text.Pandoc hiding (MathML)

forceEither :: Show e => Either e a -> a
forceEither = either (error . show) id

-- | Get configuration from config file.
getConfigFromFile :: FilePath -> IO Config
getConfigFromFile fname = do
  cp <- getDefaultConfigParser
  readfile cp fname >>= extractConfig . forceEither

-- | A version of readfile that treats the file as UTF-8.
readfile :: MonadError CPError m
          => ConfigParser
          -> FilePath
          -> IO (m ConfigParser)
readfile cp path' = do
  contents <- readFileUTF8 path'
  return $ readstring cp contents

extractConfig :: ConfigParser -> IO Config
extractConfig cp = do
  config' <- runErrorT $ do
      cfRepositoryType <- get cp "DEFAULT" "repository-type"
      cfRepositoryPath <- get cp "DEFAULT" "repository-path"
      cfDefaultPageType <- get cp "DEFAULT" "default-page-type"
      cfMathMethod <- get cp "DEFAULT" "math"
      cfShowLHSBirdTracks <- get cp "DEFAULT" "show-lhs-bird-tracks"
      cfAuthenticationMethod <- get cp "DEFAULT" "authentication-method"
      cfUserFile <- get cp "DEFAULT" "user-file"
      cfSessionTimeout <- get cp "DEFAULT" "session-timeout"
      cfTemplatesDir <- get cp "DEFAULT" "templates-dir"
      cfLogFile <- get cp "DEFAULT" "log-file"
      cfLogLevel <- get cp "DEFAULT" "log-level"
      cfStaticDir <- get cp "DEFAULT" "static-dir"
      cfPlugins <- get cp "DEFAULT" "plugins"
      cfTableOfContents <- get cp "DEFAULT" "table-of-contents"
      cfMaxUploadSize <- get cp "DEFAULT" "max-upload-size"
      cfMaxPageSize <- get cp "DEFAULT" "max-page-size"
      cfPort <- get cp "DEFAULT" "port"
      cfDebugMode <- get cp "DEFAULT" "debug-mode"
      cfFrontPage <- get cp "DEFAULT" "front-page"
      cfNoEdit <- get cp "DEFAULT" "no-edit"
      cfNoDelete <- get cp "DEFAULT" "no-delete"
      cfDefaultSummary <- get cp "DEFAULT" "default-summary"
      cfAccessQuestion <- get cp "DEFAULT" "access-question"
      cfAccessQuestionAnswers <- get cp "DEFAULT" "access-question-answers"
      cfUseRecaptcha <- get cp "DEFAULT" "use-recaptcha"
      cfRecaptchaPublicKey <- get cp "DEFAULT" "recaptcha-public-key"
      cfRecaptchaPrivateKey <- get cp "DEFAULT" "recaptcha-private-key"
      cfCompressResponses <- get cp "DEFAULT" "compress-responses"
      cfUseCache <- get cp "DEFAULT" "use-cache"
      cfCacheDir <- get cp "DEFAULT" "cache-dir"
      cfMimeTypesFile <- get cp "DEFAULT" "mime-types-file"
      cfMailCommand <- get cp "DEFAULT" "mail-command"
      cfResetPasswordMessage <- get cp "DEFAULT" "reset-password-message"
      cfUseFeed <- get cp "DEFAULT" "use-feed"
      cfBaseUrl <- get cp "DEFAULT" "base-url"
      cfWikiTitle <- get cp "DEFAULT" "wiki-title"
      cfFeedDays <- get cp "DEFAULT" "feed-days"
      cfFeedRefreshTime <- get cp "DEFAULT" "feed-refresh-time"
      cfPDFExport <- get cp "DEFAULT" "pdf-export"
      cfPandocUserData <- get cp "DEFAULT" "pandoc-user-data"
      let (pt, lhs) = parsePageType cfDefaultPageType
      let markupHelpFile = show pt ++ if lhs then "+LHS" else ""
      markupHelpPath <- liftIO $ getDataFileName $ "data" </> "markupHelp" </> markupHelpFile
      markupHelpText <- liftM (writeHtmlString defaultWriterOptions . readMarkdown defaultParserState) $
                            liftIO $ readFileUTF8 markupHelpPath

      mimeMap' <- liftIO $ readMimeTypesFile cfMimeTypesFile
      let authMethod = map toLower cfAuthenticationMethod
      let stripTrailingSlash = reverse . dropWhile (=='/') . reverse
      let repotype' = case map toLower cfRepositoryType of
                        "git"       -> Git
                        "darcs"     -> Darcs
                        "mercurial" -> Mercurial
                        x           -> error $ "Unknown repository type: " ++ x

      return $! Config{
          repositoryPath       = cfRepositoryPath
        , repositoryType       = repotype'
        , defaultPageType      = pt
        , mathMethod           = case map toLower cfMathMethod of
                                      "jsmath"   -> JsMathScript
                                      "mathml"   -> MathML
                                      _          -> RawTeX
        , defaultLHS           = lhs
        , showLHSBirdTracks    = cfShowLHSBirdTracks
        , withUser             = case authMethod of
                                      "form"     -> withUserFromSession
                                      "http"     -> withUserFromHTTPAuth
                                      _          -> id
        , authHandler          = case authMethod of
                                      "form"     -> msum formAuthHandlers
                                      "http"     -> msum httpAuthHandlers
                                      _          -> mzero
        , userFile             = cfUserFile
        , sessionTimeout       = readNumber "session-timeout" cfSessionTimeout * 60  -- convert minutes -> seconds
        , templatesDir         = cfTemplatesDir
        , logFile              = cfLogFile
        , logLevel             = let levelString = map toUpper cfLogLevel
                                     levels = ["DEBUG", "INFO", "NOTICE", "WARNING", "ERROR",
                                               "CRITICAL", "ALERT", "EMERGENCY"]
                                 in  if levelString `elem` levels
                                        then read levelString
                                        else error $ "Invalid log-level.\nLegal values are: " ++ intercalate ", " levels
        , staticDir            = cfStaticDir
        , pluginModules        = splitCommaList cfPlugins
        , tableOfContents      = cfTableOfContents
        , maxUploadSize        = readSize "max-upload-size" cfMaxUploadSize
        , maxPageSize          = readSize "max-page-size" cfMaxPageSize
        , portNumber           = readNumber "port" cfPort
        , debugMode            = cfDebugMode
        , frontPage            = cfFrontPage
        , noEdit               = splitCommaList cfNoEdit
        , noDelete             = splitCommaList cfNoDelete
        , defaultSummary       = cfDefaultSummary
        , accessQuestion       = if null cfAccessQuestion
                                    then Nothing
                                    else Just (cfAccessQuestion, splitCommaList cfAccessQuestionAnswers)
        , useRecaptcha         = cfUseRecaptcha
        , recaptchaPublicKey   = cfRecaptchaPublicKey
        , recaptchaPrivateKey  = cfRecaptchaPrivateKey
        , compressResponses    = cfCompressResponses
        , useCache             = cfUseCache
        , cacheDir             = cfCacheDir
        , mimeMap              = mimeMap'
        , mailCommand          = cfMailCommand
        , resetPasswordMessage = fromQuotedMultiline cfResetPasswordMessage
        , markupHelp           = markupHelpText
        , useFeed              = cfUseFeed
        , baseUrl              = stripTrailingSlash cfBaseUrl
        , wikiTitle            = cfWikiTitle
        , feedDays             = readNumber "feed-days" cfFeedDays
        , feedRefreshTime      = readNumber "feed-refresh-time" cfFeedRefreshTime
        , pdfExport            = cfPDFExport
        , pandocUserData       = if null cfPandocUserData
                                    then Nothing
                                    else Just cfPandocUserData }
  case config' of
        Left (ParseError e, e') -> error $ "Parse error: " ++ e ++ "\n" ++ e'
        Left e                  -> error (show e)
        Right c                 -> return c

fromQuotedMultiline :: String -> String
fromQuotedMultiline = unlines . map doline . lines . dropWhile (`elem` " \t\n")
  where doline = dropWhile (`elem` " \t") . dropGt
        dropGt ('>':' ':xs) = xs
        dropGt ('>':xs) = xs
        dropGt x = x

readNumber :: (Num a, Read a) => String -> String -> a
readNumber _   x | all isDigit x = read x
readNumber opt _ = error $ opt ++ " must be a number."

readSize :: (Num a, Read a) => String -> String -> a
readSize opt x =
  case reverse x of
       ('K':_) -> readNumber opt (init x) * 1000
       ('M':_) -> readNumber opt (init x) * 1000000
       ('G':_) -> readNumber opt (init x) * 1000000000
       _       -> readNumber opt x

splitCommaList :: String -> [String]
splitCommaList l =
  let (first,rest) = break (== ',') l
      first' = lrStrip first
  in case rest of
         []     -> if null first' then [] else [first']
         (_:rs) -> first' : splitCommaList rs

lrStrip :: String -> String
lrStrip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` " \t\n")

getDefaultConfigParser :: IO ConfigParser
getDefaultConfigParser = do
  cp <- getDataFileName "data/default.conf" >>= readfile emptyCP
  return $ forceEither cp

-- | Returns the default gitit configuration.
getDefaultConfig :: IO Config
getDefaultConfig = getDefaultConfigParser >>= extractConfig

-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String String)
readMimeTypesFile f = catch
  (liftM (foldr go M.empty . map words . lines) $ readFileUTF8 f)
  handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (\ext -> M.insert ext x) m xs
           handleMimeTypesFileNotFound e = do
             logM "gitit" WARNING $ "Could not read mime types file: " ++
               f ++ "\n" ++ show e ++ "\n" ++ "Using defaults instead."
             return mimeTypes

{-
-- | Ready collection of common mime types. (Copied from
-- Happstack.Server.HTTP.FileServe.)
mimeTypes :: M.Map String String
mimeTypes = M.fromList
        [("xml","application/xml")
        ,("xsl","application/xml")
        ,("js","text/javascript")
        ,("html","text/html")
        ,("htm","text/html")
        ,("css","text/css")
        ,("gif","image/gif")
        ,("jpg","image/jpeg")
        ,("png","image/png")
        ,("txt","text/plain")
        ,("doc","application/msword")
        ,("exe","application/octet-stream")
        ,("pdf","application/pdf")
        ,("zip","application/zip")
        ,("gz","application/x-gzip")
        ,("ps","application/postscript")
        ,("rtf","application/rtf")
        ,("wav","application/x-wav")
        ,("hs","text/plain")]
-}
