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

{- Functions for maintaining user list and session state.
-}

module Gitit.State where

import qualified Data.Map as M
import System.Random (randomRIO)
import Data.Digest.Pure.SHA (sha512, showDigest)
import qualified Data.ByteString.Lazy.UTF8 as L (fromString)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad.Trans (MonadIO(), liftIO)
import Control.Monad (replicateM, liftM)
import Control.Exception (try, throwIO)
import Data.FileStore
import Data.List (intercalate)
import Text.XHtml (Html)
import qualified Text.StringTemplate as T
import Gitit.Server (readMimeTypesFile)

appstate :: IORef AppState
appstate = unsafePerformIO $  newIORef $ AppState { sessions = undefined
                                                  , users = undefined
                                                  , config = undefined
                                                  , filestore = undefined
                                                  , mimeMap = undefined
                                                  , cache = undefined
                                                  , template = undefined
                                                  , jsMath = undefined }

initializeAppState :: MonadIO m => Config -> M.Map String User -> T.StringTemplate String -> m ()
initializeAppState conf users' templ = do
  mimeMapFromFile <- liftIO $ readMimeTypesFile (mimeTypesFile conf)
  jsMathExists <- liftIO $ doesFileExist $ staticDir conf </> "js" </> "jsMath" </> "easy" </> "load.js"
  updateAppState $ \s -> s { sessions  = Sessions M.empty
                           , users     = users'
                           , config    = conf
                           , filestore = case repository conf of
                                              Git fs   -> gitFileStore fs
                                              Darcs fs -> darcsFileStore fs
                           , mimeMap   = mimeMapFromFile
                           , cache     = M.empty
                           , template  = templ
                           , jsMath    = jsMathExists }

updateAppState :: MonadIO m => (AppState -> AppState) -> m () 
updateAppState fn = liftIO $! atomicModifyIORef appstate $ \st -> (fn st, ())

queryAppState :: MonadIO m => (AppState -> a) -> m a
queryAppState fn = liftIO $! readIORef appstate >>= return . fn

data Repository = Git FilePath 
                | Darcs FilePath 
                deriving (Read, Show)

data PageType = Markdown | RST
                deriving (Read, Show)

-- | Data structure for information read from config file.
data Config = Config {
  repository          :: Repository,               -- file store for pages
  defaultPageType     :: PageType,                 -- the default page markup type for this wiki
  userFile            :: FilePath,                 -- path of users database 
  templateFile        :: FilePath,                 -- path of page template file
  staticDir           :: FilePath,                 -- path of static directory
  tableOfContents     :: Bool,                     -- should each page have an automatic table of contents?
  maxUploadSize       :: Integer,                  -- maximum size of pages and file uploads
  portNumber          :: Int,                      -- port number to serve content on
  debugMode           :: Bool,                     -- should debug info be printed to the console?
  frontPage           :: String,                   -- the front page of the wiki
  noEdit              :: [String],                 -- pages that cannot be edited through the web interface
  noDelete            :: [String],                 -- pages that cannot be deleted through the web interface
  accessQuestion      :: Maybe (String, [String]), -- if Nothing, then anyone can register for an account.
                                                   -- if Just (prompt, answers), then a user will be given the prompt
                                                   -- and must give one of the answers in order to register.
  useRecaptcha        :: Bool,                     -- use ReCAPTCHA service to provide captchas for user registration.
  recaptchaPublicKey  :: String,
  recaptchaPrivateKey :: String,
  mimeTypesFile       :: FilePath                  -- path of file associating mime types with file extensions
  } deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config {
  repository          = Git "wikidata",
  defaultPageType     = Markdown,
  userFile            = "gitit-users",
  templateFile        = "template.html",
  staticDir           = "static",
  tableOfContents     = True,
  maxUploadSize       = 100000,
  portNumber          = 5001,
  debugMode           = False,
  frontPage           = "Front Page",
  noEdit              = ["Help"],
  noDelete            = ["Help", "Front Page"],
  accessQuestion      = Nothing,
  useRecaptcha        = False,
  recaptchaPublicKey  = "",
  recaptchaPrivateKey = "",
  mimeTypesFile       = "/etc/mime.types"
  }

data CachedPage = CachedPage {
    cpContents        :: Html
  , cpRevisionId      :: RevisionId
  } deriving Show

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

data AppState = AppState {
  sessions  :: Sessions SessionData,
  users     :: M.Map String User,
  config    :: Config,
  filestore :: FileStore,
  mimeMap   :: M.Map String String,
  cache     :: M.Map String CachedPage,
  template  :: T.StringTemplate String,
  jsMath    :: Bool
}

lookupCache :: MonadIO m => String -> (Maybe RevisionId) -> m (Maybe CachedPage)
lookupCache file (Just revid) = do
  c <- queryAppState cache
  fs <- getFileStore
  case M.lookup file c of
       Just cp | idsMatch fs (cpRevisionId cp) revid ->
                   return $ Just cp
       _        -> return Nothing
lookupCache file Nothing = do
  fs <- getFileStore
  latestRes <- liftIO $ try (latest fs file)
  case latestRes of
       Right latestid -> do
         c <- queryAppState cache
         case M.lookup file c of
              Just cp | idsMatch fs (cpRevisionId cp) latestid ->
                          return $ Just cp
              _        -> return Nothing
       Left NotFound   -> return Nothing
       Left e          -> liftIO $ throwIO e

cacheContents :: MonadIO m => String -> RevisionId -> Html -> m ()
cacheContents file revid contents = do
  c <- queryAppState cache
  let newpage = CachedPage { cpContents = contents
                           , cpRevisionId = revid }
  let newcache = M.insert file newpage c
  updateAppState $ \s -> s { cache = newcache }

mkUser :: String   -- username
       -> String   -- email
       -> String   -- unhashed password
       -> IO User
mkUser uname email pass = do
  salt <- genSalt
  return $ User { uUsername = uname,
                  uPassword = Password { pSalt = salt, pHashed = hashPassword salt pass },
                  uEmail = email }

genSalt :: IO String
genSalt = replicateM 32 $ randomRIO ('0','z')

hashPassword :: String -> String -> String
hashPassword salt pass = showDigest $ sha512 $ L.fromString $ salt ++ pass

authUser :: MonadIO m => String -> String -> m Bool
authUser name pass = do
  users' <- queryAppState users
  case M.lookup name users' of
       Just u  -> do
         let salt = pSalt $ uPassword u
         let hashed = pHashed $ uPassword u
         return $ hashed == hashPassword salt pass
       Nothing -> return False 

isUser :: MonadIO m => String -> m Bool
isUser name = liftM (M.member name) $ queryAppState users

addUser :: MonadIO m => String -> User -> m () 
addUser uname user = updateAppState (\s -> s { users = M.insert uname user (users s) }) >>
                     liftIO writeUserFile

delUser :: MonadIO m => String -> m ()
delUser uname = updateAppState (\s -> s { users = M.delete uname (users s) }) >>
                liftIO writeUserFile

writeUserFile :: IO ()
writeUserFile = do
  conf <- getConfig
  usrs <- queryAppState users
  liftIO $ writeFile (userFile conf) $ "[" ++ intercalate "\n," (map show $ M.toList usrs) ++ "\n]"

getUser :: MonadIO m => String -> m (Maybe User)
getUser uname = liftM (M.lookup uname) $ queryAppState users

isSession :: MonadIO m => SessionKey -> m Bool
isSession key = liftM (M.member key . unsession) $ queryAppState sessions

setSession :: MonadIO m => SessionKey -> SessionData -> m ()
setSession key u = updateAppState $ \s -> s { sessions = Sessions . M.insert key u . unsession $ sessions s }

newSession :: MonadIO m => SessionData -> m SessionKey
newSession u = do
  key <- liftIO $ randomRIO (0, 1000000000)
  setSession key u
  return key

delSession :: MonadIO m => SessionKey -> m ()
delSession key = updateAppState $ \s -> s { sessions = Sessions . M.delete key . unsession $ sessions s }

getSession :: MonadIO m => SessionKey -> m (Maybe SessionData)
getSession key = queryAppState $ M.lookup key . unsession . sessions

getConfig :: MonadIO m => m Config
getConfig = queryAppState config

getFileStore :: MonadIO m => m FileStore
getFileStore = queryAppState filestore

getDefaultPageType :: MonadIO m => m PageType
getDefaultPageType = liftM defaultPageType (queryAppState config)
