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
import qualified Data.ByteString.UTF8 as B (fromString, toString, length)
import System.Time (getClockTime)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (MonadIO(), liftIO)
import Control.Monad (replicateM, liftM, when)
import Control.Exception (try, throwIO)
import Data.FileStore
import Data.List (intercalate, minimumBy)
import Data.Ord (comparing)
import Text.XHtml (Html, renderHtmlFragment, primHtml)
import System.Log.Logger (Priority(..), logM)
import Gitit.Types

appstate :: IORef AppState
appstate = unsafePerformIO $  newIORef  AppState { sessions = undefined
                                                 , users = undefined
                                                 , config = undefined
                                                 , mimeMap = undefined
                                                 , cache = undefined
                                                 , plugins = undefined }

initializeAppState :: MonadIO m
                   => Config
                   -> M.Map String User
                   -> m ()
initializeAppState conf users' = do
  mimeMapFromFile <- liftIO $ readMimeTypesFile (mimeTypesFile conf)
  updateAppState $ \s -> s { sessions  = Sessions M.empty
                           , users     = users'
                           , config    = conf
                           , mimeMap   = mimeMapFromFile
                           , cache     = emptyCache
                           , plugins   = [] }

-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String String)
readMimeTypesFile f = catch
  (liftM (foldr go M.empty . map words . lines) $ readFile f)
  handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (\ext m' -> M.insert ext x m') m xs
           handleMimeTypesFileNotFound e = do
             logM "gitit" WARNING $ "Could not read mime types file: " ++
               f ++ "\n" ++ show e ++ "\n" ++ "Using defaults instead."
             return mimeTypes

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

updateAppState :: MonadIO m => (AppState -> AppState) -> m ()
updateAppState fn = liftIO $! atomicModifyIORef appstate $ \st -> (fn st, ())

queryAppState :: MonadIO m => (AppState -> a) -> m a
queryAppState fn = liftM fn $ liftIO $! readIORef appstate

emptyCache :: Cache
emptyCache = Cache M.empty 0

debugMessage :: MonadIO m => String -> m ()
debugMessage msg = do
  level <- liftM logLevel getConfig
  when (level == DEBUG) $ liftIO $ logM "gitit" DEBUG msg

updateCachedPageTimestamp :: MonadIO m => Cache -> String -> m ()
updateCachedPageTimestamp cache' page = do
  now <- liftIO getClockTime
  let setTimeStamp Nothing   = Nothing
      setTimeStamp (Just cp) = Just cp{ cpLastAccessTime = now }
  let newcache = cache'{ cachePages = 
                     M.alter setTimeStamp page (cachePages cache') }
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
     then debugMessage $ "Not caching page " ++ file ++
                        " because it is bigger than the maximum cache size."
     else do
       now <- liftIO getClockTime
       let newpage = CachedPage { cpContents       = contentsBS
                                , cpRevisionId     = revid
                                , cpLastAccessTime = now }
       let newcache = c{ cachePages = M.insert file newpage (cachePages c),
                         cacheSize  = cacheSize c + newsize - oldsize }
       newcachePruned <- pruneCache maxCacheSize' newcache
       debugMessage $ "Updating cache with " ++ file ++
                     ".  Total cache size = " ++ show (cacheSize newcachePruned)
       updateAppState $ \s -> s { cache = newcachePruned }

pruneCache :: MonadIO m => Integer -> Cache -> m Cache
pruneCache maxSize c =
  if cacheSize c < maxSize
     then return c
     else dropOldest c >>= pruneCache maxSize

dropOldest :: MonadIO m => Cache -> m Cache
dropOldest c = do
  let pgs    = M.toList $ cachePages c
  let (oldestFile, oldestCp) = minimumBy (comparing (cpLastAccessTime . snd))
                                pgs
  let oldestSize = fromIntegral $ B.length $ cpContents oldestCp
  debugMessage $ "Removing " ++ oldestFile ++ " (" ++ show oldestSize ++
                 " bytes) from cache to keep size under limit."
  return $ c{ cachePages = M.delete oldestFile (cachePages c),
              cacheSize = cacheSize c - oldestSize }

mkUser :: String   -- username
       -> String   -- email
       -> String   -- unhashed password
       -> IO User
mkUser uname email pass = do
  salt <- genSalt
  return  User { uUsername = uname,
                 uPassword = Password { pSalt = salt,
                                        pHashed = hashPassword salt pass },
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
addUser uname user =
  updateAppState (\s -> s { users = M.insert uname user (users s) }) >>
  liftIO writeUserFile

adjustUser :: MonadIO m => String -> User -> m ()
adjustUser uname user = updateAppState
  (\s -> s  { users = M.adjust (const user) uname (users s) }) >>
  liftIO writeUserFile

delUser :: MonadIO m => String -> m ()
delUser uname =
  updateAppState (\s -> s { users = M.delete uname (users s) }) >>
  liftIO writeUserFile

writeUserFile :: IO ()
writeUserFile = do
  conf <- getConfig
  usrs <- queryAppState users
  liftIO $ writeFile (userFile conf) $
           "[" ++ intercalate "\n," (map show $ M.toList usrs) ++ "\n]"

getUser :: MonadIO m => String -> m (Maybe User)
getUser uname = liftM (M.lookup uname) $ queryAppState users

isSession :: MonadIO m => SessionKey -> m Bool
isSession key = liftM (M.member key . unsession) $ queryAppState sessions

setSession :: MonadIO m => SessionKey -> SessionData -> m ()
setSession key u = updateAppState $ \s ->
  s { sessions = Sessions . M.insert key u . unsession $ sessions s }

newSession :: MonadIO m => SessionData -> m SessionKey
newSession u = do
  key <- liftIO $ randomRIO (0, 1000000000)
  setSession key u
  return key

delSession :: MonadIO m => SessionKey -> m ()
delSession key = updateAppState $ \s ->
  s { sessions = Sessions . M.delete key . unsession $ sessions s }

getSession :: MonadIO m => SessionKey -> m (Maybe SessionData)
getSession key = queryAppState $ M.lookup key . unsession . sessions

getConfig :: MonadIO m => m Config
getConfig = queryAppState config

getFileStore :: MonadIO m => m FileStore
getFileStore = liftM filestore getConfig

getDefaultPageType :: MonadIO m => m PageType
getDefaultPageType = liftM defaultPageType (queryAppState config)

getDefaultLHS :: MonadIO m => m Bool
getDefaultLHS = liftM defaultLHS (queryAppState config)
