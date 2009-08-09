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

module Network.Gitit.State where

import qualified Data.Map as M
import System.Random (randomRIO)
import Data.Digest.Pure.SHA (sha512, showDigest)
import qualified Data.ByteString.Lazy.UTF8 as L (fromString)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader
import Data.FileStore
import Data.List (intercalate)
import System.Log.Logger (Priority(..), logM)
import Network.Gitit.Types

gititstate :: IORef GititState
gititstate = unsafePerformIO $  newIORef  GititState { sessions = undefined
                                                     , users = undefined
                                                     , templatesPath = undefined
                                                     , renderPage = undefined
                                                     , plugins = undefined }

updateGititState :: MonadIO m => (GititState -> GititState) -> m ()
updateGititState fn = liftIO $! atomicModifyIORef gititstate $ \st -> (fn st, ())

queryGititState :: MonadIO m => (GititState -> a) -> m a
queryGititState fn = liftM fn $ liftIO $! readIORef gititstate

debugMessage :: String -> GititServerPart ()
debugMessage msg = liftIO $ logM "gitit" DEBUG msg

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

authUser :: String -> String -> GititServerPart Bool
authUser name pass = do
  users' <- queryGititState users
  case M.lookup name users' of
       Just u  -> do
         let salt = pSalt $ uPassword u
         let hashed = pHashed $ uPassword u
         return $ hashed == hashPassword salt pass
       Nothing -> return False

isUser :: String -> GititServerPart Bool
isUser name = liftM (M.member name) $ queryGititState users

addUser :: String -> User -> GititServerPart ()
addUser uname user =
  updateGititState (\s -> s { users = M.insert uname user (users s) }) >>
  getConfig >>=
  liftIO . writeUserFile

adjustUser :: String -> User -> GititServerPart ()
adjustUser uname user = updateGititState
  (\s -> s  { users = M.adjust (const user) uname (users s) }) >>
  getConfig >>=
  liftIO . writeUserFile

delUser :: String -> GititServerPart ()
delUser uname =
  updateGititState (\s -> s { users = M.delete uname (users s) }) >>
  getConfig >>=
  liftIO . writeUserFile

writeUserFile :: Config -> IO ()
writeUserFile conf = do
  usrs <- queryGititState users
  writeFile (userFile conf) $
      "[" ++ intercalate "\n," (map show $ M.toList usrs) ++ "\n]"

getUser :: String -> GititServerPart (Maybe User)
getUser uname = liftM (M.lookup uname) $ queryGititState users

isSession :: MonadIO m => SessionKey -> m Bool
isSession key = liftM (M.member key . unsession) $ queryGititState sessions

setSession :: MonadIO m => SessionKey -> SessionData -> m ()
setSession key u = updateGititState $ \s ->
  s { sessions = Sessions . M.insert key u . unsession $ sessions s }

newSession :: MonadIO m => SessionData -> m SessionKey
newSession u = do
  key <- liftIO $ randomRIO (0, 1000000000)
  setSession key u
  return key

delSession :: MonadIO m => SessionKey -> m ()
delSession key = updateGititState $ \s ->
  s { sessions = Sessions . M.delete key . unsession $ sessions s }

getSession :: MonadIO m => SessionKey -> m (Maybe SessionData)
getSession key = queryGititState $ M.lookup key . unsession . sessions

getConfig :: GititServerPart Config
getConfig = liftM wikiConfig ask

getFileStore :: GititServerPart FileStore
getFileStore = liftM wikiFileStore ask

getDefaultPageType :: GititServerPart PageType
getDefaultPageType = liftM defaultPageType getConfig

getDefaultLHS :: GititServerPart Bool
getDefaultLHS = liftM defaultLHS getConfig
