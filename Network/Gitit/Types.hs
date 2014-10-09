{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>,
Anton van Straaten <anton@appsolutions.com>

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

{- | Types for Gitit modules.
-}

module Network.Gitit.Types (
                            PageType(..)
                           , FileStoreType(..)
                           , MathMethod(..)
                           , AuthenticationLevel(..)
                           , Config(..)
                           , Page(..)
                           , SessionKey
                           -- we do not export SessionData constructors, in case we need to extend  SessionData with other data in the future
                           , SessionData
                           , sessionData
                           , sessionDataGithubState
                           , sessionUser
                           , sessionGithubState
                           , User(..)
                           , Sessions(..)
                           , Password(..)
                           , GititState(..)
                           , HasContext
                           , modifyContext
                           , getContext
                           , ContentTransformer
                           , Plugin(..)
                           , PluginData(..)
                           , PluginM
                           , runPluginM
                           , Context(..)
                           , PageLayout(..)
                           , Tab(..)
                           , Recaptcha(..)
                           , Params(..)
                           , Command(..)
                           , WikiState(..)
                           , GititServerPart
                           , Handler
                           , fromEntities
                           , GithubConfig
                           , oAuth2
                           , org
                           , githubConfig) where

import Control.Monad.Reader (ReaderT, runReaderT, mplus)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad (liftM)
import System.Log.Logger (Priority(..))
import Text.Pandoc.Definition (Pandoc)
import Text.XHtml (Html)
import qualified Data.Map as M
import Data.Text (Text)
import Data.List (intersect)
import Data.Time (parseTime)
import System.Locale (defaultTimeLocale)
import Data.FileStore.Types
import Network.Gitit.Server
import Text.HTML.TagSoup.Entity (lookupEntity)
import Data.Char (isSpace)
import Network.OAuth.OAuth2

data PageType = Markdown | RST | LaTeX | HTML | Textile | Org | DocBook 
              | MediaWiki
                deriving (Read, Show, Eq)

data FileStoreType = Git | Darcs | Mercurial deriving Show

data MathMethod = MathML | JsMathScript | WebTeX String | RawTeX | MathJax String
                  deriving (Read, Show, Eq)

data AuthenticationLevel = Never | ForModify | ForRead
                  deriving (Read, Show, Eq, Ord)

-- | Data structure for information read from config file.
data Config = Config {
  -- | Path of repository containing filestore
  repositoryPath       :: FilePath,
  -- | Type of repository
  repositoryType       :: FileStoreType,
  -- | Default page markup type for this wiki
  defaultPageType      :: PageType,
  -- | How to handle LaTeX math in pages?
  mathMethod           :: MathMethod,
  -- | Treat as literate haskell by default?
  defaultLHS           :: Bool,
  -- | Show Haskell code with bird tracks
  showLHSBirdTracks    :: Bool,
  -- | Combinator to set @REMOTE_USER@ request header
  withUser             :: Handler -> Handler,
  -- | Handler for login, logout, register, etc.
  requireAuthentication :: AuthenticationLevel,
  -- | Specifies which actions require authentication.
  authHandler          :: Handler,
  -- | Path of users database
  userFile             :: FilePath,
  -- | Seconds of inactivity before session expires
  sessionTimeout       :: Int,
  -- | Directory containing page templates
  templatesDir         :: FilePath,
  -- | Path of server log file
  logFile              :: FilePath,
  -- | Severity filter for log messages (DEBUG, INFO,
  -- NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY)
  logLevel             :: Priority,
  -- | Path of static directory
  staticDir            :: FilePath,
  -- | Names of plugin modules to load
  pluginModules        :: [String],
  -- | Show table of contents on each page?
  tableOfContents      :: Bool,
  -- | Max size of file uploads
  maxUploadSize        :: Integer,
  -- | Max size of page uploads
  maxPageSize          :: Integer,
  -- | IP address to bind to
  address              :: String,
  -- | Port number to serve content on
  portNumber           :: Int,
  -- | Print debug info to the console?
  debugMode            :: Bool,
  -- | The front page of the wiki
  frontPage            :: String,
  -- | Pages that cannot be edited via web
  noEdit               :: [String],
  -- | Pages that cannot be deleted via web
  noDelete             :: [String],
  -- | Default summary if description left blank
  defaultSummary       :: String,
  -- | @Nothing@ = anyone can register.
  -- @Just (prompt, answers)@ = a user will
  -- be given the prompt and must give
  -- one of the answers to register.
  accessQuestion       :: Maybe (String, [String]),
  -- | Use ReCAPTCHA for user registration.
  useRecaptcha         :: Bool,
  recaptchaPublicKey   :: String,
  recaptchaPrivateKey  :: String,
  -- | RPX domain and key
  rpxDomain            :: String,
  rpxKey               :: String,
  -- | Should responses be compressed?
  compressResponses    :: Bool,
  -- | Should responses be cached?
  useCache             :: Bool,
  -- | Directory to hold cached pages
  cacheDir             :: FilePath,
  -- | Map associating mime types with file extensions
  mimeMap              :: M.Map String String,
  -- | Command to send notification emails
  mailCommand          :: String,
  -- | Text of password reset email
  resetPasswordMessage :: String,
  -- | Markup syntax help for edit sidebar
  markupHelp           :: String,
  -- | Provide an atom feed?
  useFeed              :: Bool,
  -- | Base URL of wiki, for use in feed
  baseUrl              :: String,
  -- | Title of wiki, used in feed
  useAbsoluteUrls      :: Bool,
  -- | Should WikiLinks be absolute w.r.t. the base URL?
  wikiTitle            :: String,
  -- | Number of days history to be included in feed
  feedDays             :: Integer,
  -- | Number of minutes to cache feeds before refreshing
  feedRefreshTime      :: Integer,
  -- | Allow PDF export?
  pdfExport            :: Bool,
  -- | Directory to search for pandoc customizations
  pandocUserData       :: Maybe FilePath,
  -- | Filter HTML through xss-sanitize
  xssSanitize          :: Bool,
  -- | The default number of days in the past to look for \"recent\" activity
  recentActivityDays   :: Int,
  -- | Github client data for authentication (id, secret, callback,
  -- authorize endpoint, access token endpoint)
  githubAuth           :: GithubConfig
  }

-- | Data for rendering a wiki page.
data Page = Page {
    pageName        :: String
  , pageFormat      :: PageType
  , pageLHS         :: Bool
  , pageTOC         :: Bool
  , pageTitle       :: String
  , pageCategories  :: [String]
  , pageText        :: String
  , pageMeta        :: [(String, String)]
} deriving (Read, Show)

type SessionKey = Integer

data SessionData = SessionData {
  sessionUser :: Maybe String,
  sessionGithubState :: Maybe String
} deriving (Read,Show,Eq)

sessionData :: String -> SessionData
sessionData user = SessionData (Just user) Nothing

sessionDataGithubState  :: String -> SessionData
sessionDataGithubState  githubState = SessionData Nothing (Just githubState)

data Sessions a = Sessions {unsession::M.Map SessionKey a}
  deriving (Read,Show,Eq)

-- Password salt hashedPassword
data Password = Password { pSalt :: String, pHashed :: String }
  deriving (Read,Show,Eq)

data User = User {
  uUsername :: String,
  uPassword :: Password,
  uEmail    :: String
} deriving (Show,Read)

-- | Common state for all gitit wikis in an application.
data GititState = GititState {
  sessions       :: Sessions SessionData,
  users          :: M.Map String User,
  templatesPath  :: FilePath,
  renderPage     :: PageLayout -> Html -> Handler,
  plugins        :: [Plugin]
}

type ContentTransformer = StateT Context GititServerPart

data Plugin = PageTransform (Pandoc -> PluginM Pandoc)
            | PreParseTransform (String -> PluginM String)
            | PreCommitTransform (String -> PluginM String)

data PluginData = PluginData { pluginConfig    :: Config
                             , pluginUser      :: Maybe User
                             , pluginRequest   :: Request
                             , pluginFileStore :: FileStore
                             }

type PluginM = ReaderT PluginData (StateT Context IO)

runPluginM :: PluginM a -> PluginData -> Context -> IO (a, Context)
runPluginM plugin = runStateT . runReaderT plugin

data Context = Context { ctxFile            :: String
                       , ctxLayout          :: PageLayout
                       , ctxCacheable       :: Bool
                       , ctxTOC             :: Bool
                       , ctxBirdTracks      :: Bool
                       , ctxCategories      :: [String]
                       , ctxMeta            :: [(String, String)]
                       }

class (Monad m) => HasContext m where
  getContext    :: m Context
  modifyContext :: (Context -> Context) -> m ()

instance HasContext ContentTransformer where
  getContext    = get
  modifyContext = modify

instance HasContext PluginM where
  getContext    = get
  modifyContext = modify

-- | Abstract representation of page layout (tabs, scripts, etc.)
data PageLayout = PageLayout
  { pgPageName       :: String
  , pgRevision       :: Maybe String
  , pgPrintable      :: Bool
  , pgMessages       :: [String]
  , pgTitle          :: String
  , pgScripts        :: [String]
  , pgShowPageTools  :: Bool
  , pgShowSiteNav    :: Bool
  , pgMarkupHelp     :: Maybe String
  , pgTabs           :: [Tab]
  , pgSelectedTab    :: Tab
  , pgLinkToFeed     :: Bool
  }

data Tab = ViewTab
         | EditTab
         | HistoryTab
         | DiscussTab
         | DiffTab
         deriving (Eq, Show)

data Recaptcha = Recaptcha {
    recaptchaChallengeField :: String
  , recaptchaResponseField  :: String
  } deriving (Read, Show)

instance FromData SessionKey where
     fromData = readCookieValue "sid"

data Params = Params { pUsername     :: String
                     , pPassword     :: String
                     , pPassword2    :: String
                     , pRevision     :: Maybe String
                     , pDestination  :: String
                     , pForUser      :: Maybe String
                     , pSince        :: Maybe UTCTime
                     , pRaw          :: String
                     , pLimit        :: Int
                     , pPatterns     :: [String]
                     , pGotoPage     :: String
                     , pFileToDelete :: String
                     , pEditedText   :: Maybe String
                     , pMessages     :: [String]
                     , pFrom         :: Maybe String
                     , pTo           :: Maybe String
                     , pFormat       :: String
                     , pSHA1         :: String
                     , pLogMsg       :: String
                     , pEmail        :: String
                     , pFullName     :: String
                     , pAccessCode   :: String
                     , pWikiname     :: String
                     , pPrintable    :: Bool
                     , pOverwrite    :: Bool
                     , pFilename     :: String
                     , pFilePath     :: FilePath
                     , pConfirm      :: Bool
                     , pSessionKey   :: Maybe SessionKey
                     , pRecaptcha    :: Recaptcha
                     , pResetCode    :: String
                     , pRedirect     :: Maybe Bool
                     }  deriving Show

instance FromReqURI [String] where
  fromReqURI s = case fromReqURI s of
                      Just (s' :: String) ->
                                   case reads s' of
                                        ((xs,""):_) -> xs
                                        _           -> Nothing
                      Nothing             -> Nothing

instance FromData Params where
     fromData = do
         let look' = look
         un <- look' "username"       `mplus` return ""
         pw <- look' "password"       `mplus` return ""
         p2 <- look' "password2"      `mplus` return ""
         rv <- (look' "revision" >>= \s ->
                 return (if null s then Nothing else Just s))
                 `mplus` return Nothing
         fu <- liftM Just (look' "forUser") `mplus` return Nothing
         si <- liftM (parseTime defaultTimeLocale "%Y-%m-%d") (look' "since")
                 `mplus` return Nothing  -- YYYY-mm-dd format
         ds <- look' "destination" `mplus` return ""
         ra <- look' "raw"            `mplus` return ""
         lt <- lookRead "limit"       `mplus` return 100
         pa <- look' "patterns"       `mplus` return ""
         gt <- look' "gotopage"       `mplus` return ""
         ft <- look' "filetodelete"   `mplus` return ""
         me <- looks "message"
         fm <- liftM Just (look' "from") `mplus` return Nothing
         to <- liftM Just (look' "to")   `mplus` return Nothing
         et <- liftM (Just . filter (/='\r')) (look' "editedText")
                 `mplus` return Nothing
         fo <- look' "format"         `mplus` return ""
         sh <- look' "sha1"           `mplus` return ""
         lm <- look' "logMsg"         `mplus` return ""
         em <- look' "email"          `mplus` return ""
         na <- look' "full_name_1"    `mplus` return ""
         wn <- look' "wikiname"       `mplus` return ""
         pr <- (look' "printable" >> return True) `mplus` return False
         ow <- liftM (=="yes") (look' "overwrite") `mplus` return False
         fileparams <- liftM Just (lookFile "file") `mplus` return Nothing
         let (fp, fn) = case fileparams of
                             Just (x,y,_) -> (x,y)
                             Nothing      -> ("","")
         ac <- look' "accessCode"     `mplus` return ""
         cn <- (look' "confirm" >> return True) `mplus` return False
         sk <- liftM Just (readCookieValue "sid") `mplus` return Nothing
         rc <- look' "recaptcha_challenge_field" `mplus` return ""
         rr <- look' "recaptcha_response_field" `mplus` return ""
         rk <- look' "reset_code" `mplus` return ""
         rd <- (look' "redirect" >>= \r -> return (case r of
             "yes" -> Just True
             "no" -> Just False
             _ -> Nothing)) `mplus` return Nothing
         return   Params { pUsername     = un
                         , pPassword     = pw
                         , pPassword2    = p2
                         , pRevision     = rv
                         , pForUser      = fu
                         , pSince        = si
                         , pDestination  = ds
                         , pRaw          = ra
                         , pLimit        = lt
                         , pPatterns     = words pa
                         , pGotoPage     = gt
                         , pFileToDelete = ft
                         , pMessages     = me
                         , pFrom         = fm
                         , pTo           = to
                         , pEditedText   = et
                         , pFormat       = fo
                         , pSHA1         = sh
                         , pLogMsg       = lm
                         , pEmail        = em
                         , pFullName     = na
                         , pWikiname     = wn
                         , pPrintable    = pr
                         , pOverwrite    = ow
                         , pFilename     = fn
                         , pFilePath     = fp
                         , pAccessCode   = ac
                         , pConfirm      = cn
                         , pSessionKey   = sk
                         , pRecaptcha    = Recaptcha {
                              recaptchaChallengeField = rc,
                              recaptchaResponseField = rr }
                         , pResetCode    = rk
                         , pRedirect     = rd
                         }

data Command = Command (Maybe String) deriving Show

instance FromData Command where
     fromData = do
       pairs <- lookPairs
       return $ case map fst pairs `intersect` commandList of
                 []          -> Command Nothing
                 (c:_)       -> Command $ Just c
               where commandList = ["update", "cancel", "export"]

-- | State for a single wiki.
data WikiState = WikiState {
                     wikiConfig    :: Config
                   , wikiFileStore :: FileStore
                   }

type GititServerPart = ServerPartT (ReaderT WikiState IO)

type Handler = GititServerPart Response

-- Unescapes XML entities
fromEntities :: String -> String
fromEntities ('&':xs) =
  case lookupEntity ent of
        Just c  -> c ++ fromEntities rest
        Nothing -> '&' : fromEntities xs
    where (ent, rest) = case break (\c -> isSpace c || c == ';') xs of
                             (zs,';':ys) -> (zs,ys)
                             _           -> ("",xs)
fromEntities (x:xs) = x : fromEntities xs
fromEntities [] = []

data GithubConfig = GithubConfig { oAuth2 :: OAuth2
                                 , org :: Maybe Text
                                 }

githubConfig :: OAuth2 -> Maybe Text -> GithubConfig
githubConfig = GithubConfig
