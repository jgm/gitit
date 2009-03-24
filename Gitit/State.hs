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
import qualified Data.ByteString.UTF8 as B (ByteString, fromString, toString, length)
import System.Time (getClockTime, ClockTime)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad.Trans (MonadIO(), liftIO)
import Control.Monad (replicateM, liftM, when)
import Control.Exception (try, throwIO)
import Data.FileStore
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Text.XHtml (Html, renderHtmlFragment, primHtml)
import qualified Text.StringTemplate as T
import Gitit.Server (readMimeTypesFile, Web)
import Text.Pandoc (Pandoc)

appstate :: IORef AppState
appstate = unsafePerformIO $  newIORef $ AppState { sessions = undefined
                                                  , users = undefined
                                                  , config = undefined
                                                  , filestore = undefined
                                                  , mimeMap = undefined
                                                  , cache = undefined
                                                  , template = undefined
                                                  , jsMath = undefined
                                                  , plugins = undefined }

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
                           , cache     = emptyCache
                           , template  = templ
                           , jsMath    = jsMathExists
                           , plugins   = [] }

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
  pluginModules       :: [String],                 -- names of plugin modules to load
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
  maxCacheSize        :: Integer,                  -- maximum size in bytes of in-memory page cache
  mimeTypesFile       :: FilePath                  -- path of file associating mime types with file extensions
  } deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config {
  repository          = Git "wikidata",
  defaultPageType     = Markdown,
  userFile            = "gitit-users",
  templateFile        = "template.html",
  staticDir           = "static",
  pluginModules       = [],
  tableOfContents     = True,
  maxUploadSize       = 10 * 1024 * 1024,
  portNumber          = 5001,
  debugMode           = False,
  frontPage           = "Front Page",
  noEdit              = ["Help"],
  noDelete            = ["Help", "Front Page"],
  accessQuestion      = Nothing,
  useRecaptcha        = False,
  recaptchaPublicKey  = "",
  recaptchaPrivateKey = "",
  maxCacheSize        = 2 * 1024 * 1024,
  mimeTypesFile       = "/etc/mime.types"
  }

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
  sessions       :: Sessions SessionData,
  users          :: M.Map String User,
  config         :: Config,
  filestore      :: FileStore,
  mimeMap        :: M.Map String String,
  cache          :: Cache,
  template       :: T.StringTemplate String,
  jsMath         :: Bool,
  plugins        :: [Plugin]
}

-- later other types of plugin can be added
data Plugin = PageTransform (AppState -> Pandoc -> Web Pandoc)

getPageTransforms :: Web [AppState -> Pandoc -> Web Pandoc]
getPageTransforms = liftM (mapMaybe pageTransform) $ queryAppState plugins
  where pageTransform (PageTransform x) = Just x
        -- pageTransform _                 = Nothing

data CachedPage = CachedPage {
    cpContents        :: B.ByteString
  , cpRevisionId      :: RevisionId
  , cpLastAccessTime  :: ClockTime
  } deriving Show

data Cache = Cache {
    cachePages :: M.Map String CachedPage
  , cacheSize  :: Integer
}

emptyCache :: Cache
emptyCache = Cache M.empty 0

debugMessage :: MonadIO m => String -> m ()
debugMessage msg = do
  debug <- liftM debugMode getConfig
  when debug $ liftIO $ putStrLn msg

updateCachedPageTimestamp :: MonadIO m => Cache -> String -> m ()
updateCachedPageTimestamp cache' page = do
  now <- liftIO getClockTime
  let setTimeStamp Nothing   = Nothing
      setTimeStamp (Just cp) = Just cp{ cpLastAccessTime = now }
  let newcache = cache'{ cachePages = M.alter setTimeStamp page (cachePages cache') }
  updateAppState $ \s -> s {cache = newcache }

lookupCache :: MonadIO m => String -> (Maybe RevisionId) -> m (Maybe Html)
lookupCache file (Just revid) = do
  c <- queryAppState cache
  fs <- getFileStore
  case M.lookup file (cachePages c) of
       Just cp | idsMatch fs (cpRevisionId cp) revid -> do
                   debugMessage $ "Retrieving " ++ file ++ " from cache."
                   updateCachedPageTimestamp c file
                   return $ Just $ primHtml $ B.toString $ cpContents cp
       _        -> return Nothing
lookupCache file Nothing = do
  fs <- getFileStore
  latestRes <- liftIO $ try (latest fs file)
  case latestRes of
       Right latestid -> lookupCache file (Just latestid)
       Left NotFound   -> return Nothing
       Left e          -> liftIO $ throwIO e

cacheContents :: MonadIO m => String -> RevisionId -> Html -> m ()
cacheContents file revid contents = do
  c <- queryAppState cache
  let oldsize = case M.lookup file (cachePages c) of
                     Just cp  -> fromIntegral $ B.length $ cpContents cp
                     Nothing  -> 0
  let contentsBS = B.fromString $! renderHtmlFragment contents
  let newsize = fromIntegral (B.length contentsBS)
  maxCacheSize' <- liftM maxCacheSize getConfig
  if newsize > maxCacheSize'
     then debugMessage $ "Not caching page " ++ file ++ " because it is bigger than the maximum cache size."
     else do
       now <- liftIO getClockTime
       let newpage = CachedPage { cpContents       = contentsBS
                                , cpRevisionId     = revid
                                , cpLastAccessTime = now }
       let newcache = c{ cachePages = M.insert file newpage (cachePages c), cacheSize  = cacheSize c + newsize - oldsize }
       newcachePruned <- pruneCache maxCacheSize' newcache
       debugMessage $ "Updating cache with " ++ file ++ ".  Total cache size = " ++ show (cacheSize newcachePruned)
       updateAppState $ \s -> s { cache = newcachePruned }

pruneCache :: MonadIO m => Integer -> Cache -> m Cache
pruneCache maxSize c =
  if cacheSize c < maxSize
     then return c
     else dropOldest c >>= pruneCache maxSize

dropOldest :: MonadIO m => Cache -> m Cache
dropOldest c = do
  let pgs    = M.toList $ cachePages c
  let (oldestFile, oldestCp) = minimumBy (comparing (cpLastAccessTime . snd)) pgs
  let oldestSize = fromIntegral $ B.length $ cpContents oldestCp
  debugMessage $ "Removing " ++ oldestFile ++ " (" ++ show oldestSize ++ " bytes) from cache to keep size under limit."
  return $ c{ cachePages = M.delete oldestFile (cachePages c), cacheSize = cacheSize c - oldestSize }

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
