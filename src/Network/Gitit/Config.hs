{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
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
                            , getConfigFromFiles
                            , getDefaultConfig
                            , readMimeTypesFile )
where
import Network.Gitit.Types
import Network.Gitit.Server (mimeTypes)
import Network.Gitit.Framework
import Network.Gitit.Authentication (formAuthHandlers, rpxAuthHandlers, httpAuthHandlers, githubAuthHandlers)
import Network.Gitit.Util (parsePageType, readFileUTF8)
import System.Log.Logger (logM, Priority(..))
import System.IO (hPutStrLn, stderr)
import System.Exit (ExitCode(..), exitWith)
import Data.Either (partitionEithers)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Map as M
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (intercalate, foldl')
import Data.Char (toLower, toUpper, isAlphaNum)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Text (Text)
import Paths_gitit (getDataFileName)
import qualified Data.Text.Lazy.Builder as T.Builder
import qualified Data.Text.Lazy as T.Lazy
import qualified Data.Text.Lazy.Builder.Int as T.Builder.Int
import qualified Data.Text.Lazy.Builder.Scientific as Scientific
import System.FilePath ((</>))
import Text.Pandoc hiding (ERROR, WARNING, MathJax, MathML, WebTeX, getDataFileName)
import qualified Control.Exception as E
import Network.OAuth.OAuth2 (OAuth2(..))
import URI.ByteString (parseURI, laxURIParserOptions)
import qualified Data.ByteString.Char8 as BS
import Network.Gitit.Compat.Except
import Control.Monad
import Control.Monad.Trans
import Text.Parsec
import Text.Read (readMaybe)
import Control.Monad.Except (ExceptT(ExceptT))

-- | Get configuration from config file.
getConfigFromFile :: FilePath -> IO Config
getConfigFromFile fname = getConfigFromFiles [fname]

-- | Get configuration from config files, or default.
getConfigFromFiles :: [FilePath] -> IO Config
getConfigFromFiles fnames = do
  -- we start with default values from the data file
  cp <- getDataFileName "data/default.conf"
  cfgmap <- foldM alterConfigMapByNewFile mempty (cp : fnames)
  res <- runExceptT $ extractConfig cfgmap
  case res of
    Right conf -> pure conf
    Left e -> do
      hPutStrLn stderr ("Error parsing config:\n" <> e)
      exitWith (ExitFailure 1)

type ConfigMap = M.Map (Text, Text) Text

-- | Parse the config file and return a map from section/field to value.
-- The config file is first checked to see if it’s a valid json value.
--
-- If yes, use the json parser, otherwise use the old style parser for backwards compatibility.
--
-- The JSON format mirrors the old style format.
-- Any key that is in the outer object is put into the [DEFAULT] section.
-- Any key that is surrounded by `[` and `]` is a section name, and the keys inside are the fields in the section.
--
-- Example:
--
-- @
-- {
--   "repository-type": "git",
--   "[GitHub]": {
--     "oauthclientid": "clientid",
--     "oauthclientsecret": "client
--   }
-- }
-- @
alterConfigMapByNewFile :: ConfigMap -> FilePath -> IO ConfigMap
alterConfigMapByNewFile cfmap fname = do
  eJsonVal <- readJsonValueFromFile fname
  secs <- case eJsonVal of
    Right val ->
      case pSectionJson val of
        Left err -> do
          hPutStrLn stderr ("Error parsing gitit json config " <> fname <> ":\n" <> err)
          exitWith (ExitFailure 1)
        Right secs -> pure secs
    Left _jsonErr -> do
      eSecs <- readOldStyleConfig fname
      case eSecs of
        Left errOld -> do
          hPutStrLn stderr ("Cannot parse " <> fname <> " as valid json value; tried parsing it as old-style gitit config instead but failed:\n" <> errOld)
          exitWith (ExitFailure 1)
        Right secs -> pure secs
  pure $ alterConfigMap cfmap secs

alterConfigMap :: ConfigMap -> [Section] -> ConfigMap
alterConfigMap = foldl' go
  where
    go cfmap' (Section name fields) = foldl' (go' name) cfmap' fields
    go' name cfmap' (k,v) = M.insert (name, k) v cfmap'

readOldStyleConfig :: FilePath -> IO (Either String [Section])
readOldStyleConfig fname = do
  contents <- readFileUTF8 fname
  let contents' = "[DEFAULT]\n" <> contents
  case parseConfig fname contents' of
    Left msg -> do
      pure $ Left msg
    Right secs -> pure $ Right secs

readJsonValueFromFile :: FilePath -> IO (Either String Json.Value)
readJsonValueFromFile fname = do
  mval <- Json.eitherDecodeFileStrict fname
  pure $ case mval of
    Left err -> Left $ "Could not parse file as syntactically valid json value: " <> err
    Right val -> Right val

-- | Returns the default gitit configuration.
getDefaultConfig :: IO Config
getDefaultConfig = getConfigFromFiles []

data Section = Section Text [(Text, Text)]
  deriving (Show)

parseConfig :: FilePath -> Text -> Either String [Section]
parseConfig fname txt = either (Left . show) Right $ parse (many pSection) fname txt
data SectionJson =
  DefaultSection Text Text
  | ThisSection Text [(Text, Text)]

pSectionJson :: Json.Value -> Either String [Section]
pSectionJson (Json.Object obj) = obj & KeyMap.toList
  <&> (\case
    (asSectionKey -> Just k, v) -> ThisSection k <$> pSectionFields k v
    (k, asJsonScalarText -> Just t) -> Right $ DefaultSection (k & Key.toText) t
    (k, v) -> Left ["The value of field " <> (k & Key.toString) <> " has to be a string, but was: " <> show v]
    )
  & partitionEithers
  & \case
    ([], secs) -> secs & foldr go M.empty & M.toList <&> (\(name, fields) -> Section name fields) & Right
      where
        go (ThisSection name fields) acc = M.insert name fields acc
        go (DefaultSection k v) acc = M.insertWith (++) "DEFAULT" [(k, v)] acc
    (errs, _) -> Left $ intercalate "\n" $ concat errs
pSectionJson _ = Left "The toplevel json value has to be a json object."

-- section key starts with [ and ends with ]
asSectionKey :: KeyMap.Key -> Maybe Text
asSectionKey k = case Key.toString k of
  (x : (hasEnd ']' -> Just k')) | x == '[' -> Just $ k' & T.pack & T.toUpper
  _ -> Nothing
hasEnd :: Eq a => a -> [a] -> Maybe [a]
hasEnd _ [] = Nothing
hasEnd e xs
  | last xs == e = Just (init xs)
  | otherwise = Nothing

asJsonScalarText :: Json.Value -> Maybe Text
asJsonScalarText (Json.String t) = Just t
asJsonScalarText (Json.Number n) = Just $ T.Lazy.toStrict $ T.Builder.toLazyText formatNumber
  where
    -- The scientific builder always adds a decimal point, which we don’t want for e.g. port numbers :)
    formatNumber = if
      | Scientific.isInteger n
      , Just i <- Scientific.toBoundedInteger @Int n -> T.Builder.Int.decimal i
      | otherwise ->  n & Scientific.scientificBuilder
asJsonScalarText (Json.Bool True) = Just "true"
asJsonScalarText (Json.Bool False) = Just "false"
asJsonScalarText _ = Nothing

pSectionFields :: Text -> Json.Value -> Either [String] [(Text, Text)]
pSectionFields sec (Json.Object obj) = obj
   & KeyMap.toList
  <&> (\case
      (k, asJsonScalarText -> Just t) -> Right (k & Key.toText, t)
      (k, v) -> Left $ "In Section " <> (sec & T.unpack) <> ": The value of field " <> (k & Key.toString) <> " has to be a string, but was: " <> show v
      )
  & partitionEithers
  & \case
    ([], fields) -> Right fields
    (errs, _) -> Left errs
pSectionFields sec _ = Left [ "The section " <> (sec & T.unpack) <> " has to be a json object." ]

pSection :: Parsec Text () Section
pSection = do
  skipMany (pComment <|> (space *> spaces))
  Section <$> pSectionName <*> many pValue

pComment :: Parsec Text () ()
pComment = char '#' *> skipMany (satisfy (/= '\n')) <* newline

pKeyChar :: Parsec Text () Char
pKeyChar = satisfy (\c -> isAlphaNum c || c == '_' || c == '.' || c == '-')

pSectionName :: Parsec Text () Text
pSectionName = do
  char '['
  T.toUpper . T.pack <$> manyTill letter (char ']')

pValue :: Parsec Text () (Text, Text)
pValue = try $ do
  skipMany (pComment <|> (space *> spaces))
  k <- T.pack <$> manyTill pKeyChar (char ':')
  skipMany (oneOf " \t")
  v <- T.pack <$> manyTill anyChar newline
  skipMany (pComment <|> (space *> spaces))
  vs <- T.unlines <$> many pMultiline
  pure (T.toLower k, v <> vs)

pMultiline :: Parsec Text () Text
pMultiline = try $ do
  spaces
  char '>'
  optional (char ' ')
  T.pack <$> manyTill anyChar newline

extractConfig :: ConfigMap -> ExceptT String IO Config
extractConfig cfgmap = do
  let get name field = maybe (pure mempty) (pure . T.unpack) $ M.lookup (name, field) cfgmap
  cfRepositoryType <- get "DEFAULT" "repository-type"
  cfRepositoryPath <- get "DEFAULT" "repository-path"
  cfDefaultPageType <- get "DEFAULT" "default-page-type"
  cfDefaultExtension <- get "DEFAULT" "default-extension"
  cfMathMethod <- get "DEFAULT" "math"
  cfMathjaxScript <- get "DEFAULT" "mathjax-script"
  cfShowLHSBirdTracks <- get "DEFAULT" "show-lhs-bird-tracks" >>= readBool
  cfRequireAuthentication <- get "DEFAULT" "require-authentication"
  cfAuthenticationMethod <- get "DEFAULT" "authentication-method"
  cfUserFile <- get "DEFAULT" "user-file"
  cfSessionTimeout <- get "DEFAULT" "session-timeout" >>= readNumber
  cfTemplatesDir <- get "DEFAULT" "templates-dir"
  cfLogFile <- get "DEFAULT" "log-file"
  cfLogLevel <- get "DEFAULT" "log-level"
  cfStaticDir <- get "DEFAULT" "static-dir"
  cfPlugins <- get "DEFAULT" "plugins"
  cfTableOfContents <- get "DEFAULT" "table-of-contents" >>= readBool
  cfMaxUploadSize <- get "DEFAULT" "max-upload-size" >>= readSize
  cfMaxPageSize <- get "DEFAULT" "max-page-size" >>= readSize
  cfAddress <- get "DEFAULT" "address"
  cfPort <- get "DEFAULT" "port" >>= readNumber
  cfDebugMode <- get "DEFAULT" "debug-mode" >>= readBool
  cfFrontPage <- get "DEFAULT" "front-page"
  cfNoEdit <- get "DEFAULT" "no-edit"
  cfNoDelete <- get "DEFAULT" "no-delete"
  cfDefaultSummary <- get "DEFAULT" "default-summary"
  cfDeleteSummary <- get "DEFAULT" "delete-summary"
  cfDisableRegistration <- get "DEFAULT" "disable-registration" >>= readBool
  cfAccessQuestion <- get "DEFAULT" "access-question"
  cfAccessQuestionAnswers <- get "DEFAULT" "access-question-answers"
  cfUseRecaptcha <- get "DEFAULT" "use-recaptcha" >>= readBool
  cfRecaptchaPublicKey <- get "DEFAULT" "recaptcha-public-key"
  cfRecaptchaPrivateKey <- get "DEFAULT" "recaptcha-private-key"
  cfRPXDomain <- get "DEFAULT" "rpx-domain"
  cfRPXKey <- get "DEFAULT" "rpx-key"
  cfCompressResponses <- get "DEFAULT" "compress-responses" >>= readBool
  cfUseCache <- get "DEFAULT" "use-cache" >>= readBool
  cfCacheDir <- get "DEFAULT" "cache-dir"
  cfMimeTypesFile <- get "DEFAULT" "mime-types-file"
  cfMailCommand <- get "DEFAULT" "mail-command"
  cfResetPasswordMessage <- get "DEFAULT" "reset-password-message"
  cfUseFeed <- get "DEFAULT" "use-feed" >>= readBool
  cfBaseUrl <- get "DEFAULT" "base-url"
  cfAbsoluteUrls <- get "DEFAULT" "absolute-urls" >>= readBool
  cfWikiTitle <- get "DEFAULT" "wiki-title"
  cfFeedDays <- get "DEFAULT" "feed-days" >>= readNumber
  cfFeedRefreshTime <- get "DEFAULT" "feed-refresh-time" >>= readNumber
  cfPandocUserData <- get "DEFAULT" "pandoc-user-data"
  cfXssSanitize <- get "DEFAULT" "xss-sanitize" >>= readBool
  cfRecentActivityDays <- get "DEFAULT" "recent-activity-days" >>= readNumber
  let (pt, lhs) = parsePageType cfDefaultPageType
  let markupHelpFile = show pt ++ if lhs then "+LHS" else ""
  markupHelpPath <- liftIO $ getDataFileName $ "data" </> "markupHelp" </> markupHelpFile
  markupHelp' <- liftIO $ readFileUTF8 markupHelpPath
  markupHelpText <- liftIO $ handleError $ runPure $ do
    helpDoc <- readMarkdown def{ readerExtensions = getDefaultExtensions "markdown" } markupHelp'
    writeHtml5String def helpDoc

  mimeMap' <- liftIO $ readMimeTypesFile cfMimeTypesFile
  let authMethod = map toLower cfAuthenticationMethod
  let stripTrailingSlash = reverse . dropWhile (=='/') . reverse
  repotype' <- case map toLower cfRepositoryType of
                    "git"       -> pure Git
                    "darcs"     -> pure Darcs
                    "mercurial" -> pure Mercurial
                    x           -> throwError $ "Unknown repository type: " ++ x
  when (authMethod == "rpx" && cfRPXDomain == "") $
     liftIO $ logM "gitit" WARNING "rpx-domain is not set"

  ghConfig <- extractGithubConfig cfgmap

  when (null cfUserFile) $
     liftIO $ logM "gitit" ERROR "user-file is empty"

  return Config{
      repositoryPath       = cfRepositoryPath
    , repositoryType       = repotype'
    , defaultPageType      = pt
    , defaultExtension     = cfDefaultExtension
    , mathMethod           = case map toLower cfMathMethod of
                                  "mathml"   -> MathML
                                  "mathjax"  -> MathJax cfMathjaxScript
                                  "google"   -> WebTeX "http://chart.apis.google.com/chart?cht=tx&chl="
                                  _          -> RawTeX
    , defaultLHS           = lhs
    , showLHSBirdTracks    = cfShowLHSBirdTracks
    , withUser             = case authMethod of
                                  "form"     -> withUserFromSession
                                  "github"   -> withUserFromSession
                                  "http"     -> withUserFromHTTPAuth
                                  "rpx"      -> withUserFromSession
                                  _          -> id
    , requireAuthentication = case map toLower cfRequireAuthentication of
                                   "none"    -> Never
                                   "modify"  -> ForModify
                                   "read"    -> ForRead
                                   _         -> ForModify

    , authHandler          = case authMethod of
                                  "form"     -> msum $ formAuthHandlers cfDisableRegistration
                                  "github"   -> msum $ githubAuthHandlers ghConfig
                                  "http"     -> msum httpAuthHandlers
                                  "rpx"      -> msum rpxAuthHandlers
                                  _          -> mzero
    , userFile             = cfUserFile
    , sessionTimeout       = cfSessionTimeout * 60  -- convert minutes -> seconds
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
    , maxUploadSize        = cfMaxUploadSize
    , maxPageSize          = cfMaxPageSize
    , address              = cfAddress
    , portNumber           = cfPort
    , debugMode            = cfDebugMode
    , frontPage            = cfFrontPage
    , noEdit               = splitCommaList cfNoEdit
    , noDelete             = splitCommaList cfNoDelete
    , defaultSummary       = cfDefaultSummary
    , deleteSummary        = cfDeleteSummary
    , disableRegistration  = cfDisableRegistration
    , accessQuestion       = if null cfAccessQuestion
                                then Nothing
                                else Just (cfAccessQuestion,
                                           splitCommaList cfAccessQuestionAnswers)
    , useRecaptcha         = cfUseRecaptcha
    , recaptchaPublicKey   = cfRecaptchaPublicKey
    , recaptchaPrivateKey  = cfRecaptchaPrivateKey
    , rpxDomain            = cfRPXDomain
    , rpxKey               = cfRPXKey
    , compressResponses    = cfCompressResponses
    , useCache             = cfUseCache
    , cacheDir             = cfCacheDir
    , mimeMap              = mimeMap'
    , mailCommand          = cfMailCommand
    , resetPasswordMessage = cfResetPasswordMessage
    , markupHelp           = markupHelpText
    , useFeed              = cfUseFeed
    , baseUrl              = stripTrailingSlash cfBaseUrl
    , useAbsoluteUrls      = cfAbsoluteUrls
    , wikiTitle            = cfWikiTitle
    , feedDays             = cfFeedDays
    , feedRefreshTime      = cfFeedRefreshTime
    , pandocUserData       = if null cfPandocUserData
                                then Nothing
                                else Just cfPandocUserData
    , xssSanitize          = cfXssSanitize
    , recentActivityDays   = cfRecentActivityDays
    , githubAuth           = ghConfig
    }

extractGithubConfig ::  ConfigMap -> ExceptT String IO GithubConfig
extractGithubConfig cfgmap = do
  cfOauthClientId <- getGithubProp "oauthclientid"
  cfOauthClientSecret <- getGithubProp "oauthclientsecret"
  cfOauthCallback <- getUrlProp "oauthcallback"
  cfOauthOAuthorizeEndpoint  <- getUrlProp "oauthoauthorizeendpoint"
  cfOauthAccessTokenEndpoint <- getUrlProp "oauthaccesstokenendpoint"
  cfOrg' <- getGithubProp "github-org"
  let cfOrg = if null cfOrg'
                then Just cfOrg'
                else Nothing
  let cfgOAuth2 = OAuth2 {
                        oauth2ClientId = T.pack cfOauthClientId
                      , oauth2ClientSecret = T.pack cfOauthClientSecret
                      , oauth2RedirectUri = cfOauthCallback
                      , oauth2AuthorizeEndpoint = cfOauthOAuthorizeEndpoint
                      , oauth2TokenEndpoint = cfOauthAccessTokenEndpoint
                      }
  return $ githubConfig cfgOAuth2 $ fmap T.pack cfOrg
 where
  get name field = maybe (pure mempty) (pure . T.unpack) $ M.lookup (name, field) cfgmap
  getGithubProp = get "GITHUB"
  getUrlProp prop = getGithubProp prop >>= \s ->
                      case parseURI laxURIParserOptions (BS.pack s) of
                        Left e    -> throwError $ "couldn't parse url " ++ s
                                                     ++ " from (Github/" ++ T.unpack prop
                                                     ++ "): " ++ show e
                        Right uri -> return uri


-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String String)
readMimeTypesFile f = E.catch
  (foldr (go . words)  M.empty . lines . T.unpack <$> readFileUTF8 f)
  handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (`M.insert` x) m xs
           handleMimeTypesFileNotFound (e :: E.SomeException) = do
             logM "gitit" WARNING $ "Could not read mime types file: " ++
               f ++ "\n" ++ show e ++ "\n" ++ "Using defaults instead."
             return mimeTypes

readNumber :: (Monad m, Num a, Read a) => String -> ExceptT String m a
readNumber x = case readMaybe x of
                     Just n -> pure n
                     _ -> throwError $ "Could not parse " ++ x ++ " as an integer."

readSize :: (Monad m, Num a, Read a) => String -> ExceptT String m a
readSize [] = readNumber ""
readSize x =
  case last x of
       'K' -> (* 1000) <$> readNumber (init x)
       'M' -> (* 1000000) <$> readNumber (init x)
       'G' -> (*  1000000000) <$> readNumber (init x)
       _       -> readNumber x

splitCommaList :: String -> [String]
splitCommaList l =
  let (first,rest) = break (== ',') l
      first' = lrStrip first
  in case rest of
         []     -> [first' | not (null first')]
         (_:rs) -> first' : splitCommaList rs

lrStrip :: String -> String
lrStrip = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
    where isWhitespace = (`elem` [' ','\t','\n'])

readBool :: Monad m => String -> ExceptT String m Bool
readBool s =
  case map toLower s of
    "yes" -> pure True
    "y" -> pure True
    "no" -> pure False
    "n" -> pure False
    "true" -> pure True
    "t" -> pure True
    "false" -> pure False
    "f" -> pure False
    _ -> throwError $ "Could not read " <> s <> " as boolean"
