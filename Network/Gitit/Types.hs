{-# LANGUAGE TypeSynonymInstances #-}
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

{- Types for Gitit modules.
-}

module Network.Gitit.Types where

import System.Time (ClockTime)
import Control.Monad.Reader (ReaderT, runReaderT, mplus)
import Control.Monad.State (StateT, runStateT, get, modify)
import Control.Monad (liftM)
import qualified Text.StringTemplate as T
import System.Log.Logger (Priority(..))
import Text.Pandoc.Definition (Pandoc)
import qualified Data.ByteString.Lazy.UTF8 as L (ByteString)
import qualified Data.ByteString.Lazy as L (empty)
import qualified Data.ByteString.UTF8 as B (ByteString)
import qualified Data.Map as M
import Data.DateTime
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.FileStore.Types
import Network.Gitit.Server
import Text.Pandoc.CharacterReferences (decodeCharacterReferences)
import qualified Text.StringTemplate as T

data PageType = Markdown | RST | LaTeX | HTML
                deriving (Read, Show, Eq)

data AuthenticationMethod = FormAuth
                          | HTTPAuth
                          | CustomAuth (GititServerPart (Maybe User)) 

data FileStoreType = Git | Darcs deriving Show

-- | Data structure for information read from config file.
data Config = Config {
  repositoryPath       :: FilePath,    -- path of repository containing filestore
  repositoryType       :: FileStoreType, -- type of repository
  defaultPageType      :: PageType,    -- default page markup type for this wiki
  defaultLHS           :: Bool,        -- treat as literate haskell by default?
  showLHSBirdTracks    :: Bool,        -- show Haskell code with bird tracks
  authenticationMethod :: AuthenticationMethod, -- use forms or HTTP digest?
  userFile             :: FilePath,    -- path of users database
  templatesDir         :: FilePath,    -- directory containing page templates
  logFile              :: FilePath,    -- path of server log file
  logLevel             :: Priority,    -- severity filter for log messages
                                       -- (DEBUG, INFO, NOTICE, WARNING, ERROR,
                                       -- CRITICAL, ALERT, EMERGENCY)
  staticDir            :: FilePath,    -- path of static directory
  pluginModules        :: [String],    -- names of plugin modules to load
  tableOfContents      :: Bool,        -- show table of contents on each page?
  maxUploadSize        :: Integer,     -- max size of pages and file uploads
  portNumber           :: Int,         -- port number to serve content on
  debugMode            :: Bool,        -- print debug info to the console?
  frontPage            :: String,      -- the front page of the wiki
  noEdit               :: [String],    -- pages that cannot be edited via web
  noDelete             :: [String],    -- pages that cannot be deleted via web
  defaultSummary       :: String,      -- default summary if description left blank
  accessQuestion       :: Maybe (String, [String]),
                                       -- Nothing -> anyone can register
                                       -- Just (prompt, answers) -> a user will
                                       -- be given the prompt and must give
                                       -- one of the answers to register.
  useRecaptcha         :: Bool,        -- use ReCAPTCHA for user registration.
  recaptchaPublicKey   :: String,
  recaptchaPrivateKey  :: String,
  compressResponses    :: Bool,        -- should responses be compressed?
  maxCacheSize         :: Integer,     -- max size (bytes) of memory page cache
  mimeMap              :: M.Map String String, -- map assoc mime types with file exts
  jsMath               :: Bool,        -- true if jsMath files are present
  mailCommand          :: String,      -- command to send notification emails
  resetPasswordMessage :: String,      -- text of password reset email
  markupHelp           :: String       -- markup syntax help for edit sidebar
  }

data Page = Page {
    pageName        :: String
  , pageFormat      :: PageType
  , pageLHS         :: Bool
  , pageTOC         :: Bool
  , pageTitle       :: String
  , pageCategories  :: [String]
  , pageText        :: String
} deriving (Read, Show)

type SessionKey = Integer

data SessionData = SessionData {
  sessionUser :: String
} deriving (Read,Show,Eq)

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

data Template = Template {
        compiledTemplate   :: T.StringTemplate String
      , templatesDirectory :: FilePath }

data AppState = AppState {
  sessions       :: Sessions SessionData,
  users          :: M.Map String User,
  cache          :: Cache,
  template       :: Template,
  plugins        :: [Plugin]
}

type ContentTransformer = StateT Context GititServerPart

data Plugin = PageTransform (Pandoc -> PluginM Pandoc)
            | PreParseTransform (String -> PluginM String)
            | PreCommitTransform (String -> PluginM String)

type PluginM = ReaderT (Config, Maybe User) (StateT Context IO)

runPluginM :: PluginM a -> Config -> Maybe User -> Context -> IO (a, Context)
runPluginM plugin conf user = runStateT (runReaderT plugin (conf, user))

data Context = Context { ctxFile            :: String
                       , ctxLayout          :: PageLayout
                       , ctxParams          :: Params
                       , ctxCacheable       :: Bool
                       , ctxTOC             :: Bool
                       , ctxBirdTracks      :: Bool
                       , ctxCategories      :: [String]
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
  , pgShowMarkupHelp :: Bool
  , pgTabs           :: [Tab]
  , pgSelectedTab    :: Tab
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
                     , pUri          :: String
                     , pForUser      :: Maybe String
                     , pSince        :: Maybe DateTime
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
                     , pFileContents :: L.ByteString
                     , pConfirm      :: Bool 
                     , pSessionKey   :: Maybe SessionKey
                     , pRecaptcha    :: Recaptcha
                     , pResetCode    :: String
                     }  deriving Show

instance FromData Params where
     fromData = do
         let look' = liftM decodeCharacterReferences . look
         un <- look' "username"       `mplus` return ""
         pw <- look' "password"       `mplus` return ""
         p2 <- look' "password2"      `mplus` return ""
         rv <- (look' "revision" >>= \s ->
                 return (if null s then Nothing else Just s))
                 `mplus` return Nothing
         fu <- (liftM Just $ look' "forUser") `mplus` return Nothing
         si <- (liftM (parseDateTime "%Y-%m-%d") $ look' "since")
                 `mplus` return Nothing  -- YYYY-mm-dd format
         ds <- lookCookieValue "destination" `mplus` return "/"
         ra <- look' "raw"            `mplus` return ""
         lt <- lookRead "limit"       `mplus` return 100
         pa <- look' "patterns"       `mplus` return ""
         gt <- look' "gotopage"       `mplus` return ""
         ft <- look' "filetodelete"   `mplus` return ""
         me <- lookRead "messages"   `mplus` return [] 
         fm <- (liftM Just $ look' "from") `mplus` return Nothing
         to <- (liftM Just $ look' "to")   `mplus` return Nothing
         et <- (liftM (Just . filter (/='\r')) $ look' "editedText")
                 `mplus` return Nothing
         fo <- look' "format"         `mplus` return ""
         sh <- look' "sha1"           `mplus` return ""
         lm <- look' "logMsg"         `mplus` return ""
         em <- look' "email"          `mplus` return ""
         na <- look' "full_name_1"    `mplus` return ""
         wn <- look' "wikiname"       `mplus` return ""
         pr <- (look' "printable" >> return True) `mplus` return False
         ow <- (liftM (=="yes") $ look' "overwrite") `mplus` return False
         fn <- (liftM (fromMaybe "" . inputFilename) $ lookInput "file")
                 `mplus` return ""
         fc <- (liftM inputValue $ lookInput "file") `mplus` return L.empty
         ac <- look' "accessCode"     `mplus` return ""
         cn <- (look' "confirm" >> return True) `mplus` return False
         sk <- (liftM Just $ readCookieValue "sid") `mplus` return Nothing
         rc <- look' "recaptcha_challenge_field" `mplus` return ""
         rr <- look' "recaptcha_response_field" `mplus` return ""
         rk <- look' "reset_code" `mplus` return ""
         return   Params { pUsername     = un
                         , pPassword     = pw
                         , pPassword2    = p2
                         , pRevision     = rv
                         , pForUser      = fu
                         , pSince        = si
                         , pDestination  = ds
                         , pUri          = ""       -- gets set by handle...
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
                         , pFileContents = fc
                         , pAccessCode   = ac
                         , pConfirm      = cn
                         , pSessionKey   = sk
                         , pRecaptcha    = Recaptcha {
                              recaptchaChallengeField = rc,
                              recaptchaResponseField = rr }
                         , pResetCode    = rk
                         }

data Command = Command (Maybe String) deriving Show

instance FromData Command where
     fromData = do
       pairs <- lookPairs
       return $ case map fst pairs `intersect` commandList of
                 []          -> Command Nothing
                 (c:_)       -> Command $ Just c
               where commandList = ["page", "request", "params", "edit",
                                    "showraw", "history", "export", "diff",
                                    "cancel", "update", "delete", "discuss"]

data WikiState = WikiState { 
                     wikiConfig    :: Config
                   , wikiFileStore :: FileStore
                   }

type GititServerPart = ServerPartT (ReaderT WikiState IO)

type Handler = GititServerPart Response

data CachedPage = CachedPage {
    cpContents        :: B.ByteString
  , cpRevisionId      :: RevisionId
  , cpLastAccessTime  :: ClockTime
  } deriving Show

data Cache = Cache {
    cachePages :: M.Map (String, String) CachedPage
  , cacheSize  :: Integer
}

