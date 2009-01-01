{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell , FlexibleInstances,
             UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
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
   Parts of this code are based on http://hpaste.org/5957 mightybyte rev by 
   dbpatterson.
-}

module Gitit.State where

import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State (modify, MonadState)
import Data.Generics
import HAppS.State
import HAppS.Data
import GHC.Conc (STM)
import System.Random (randomRIO)
import Data.Digest.Pure.SHA (sha512, showDigest)
import qualified Data.ByteString.Lazy.UTF8 as L (fromString)

-- | Data structure for information read from config file.
data Config = Config {
  repositoryPath      :: FilePath,                 -- path of git repository for pages
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
  recaptchaPrivateKey :: String
  } deriving (Read, Show,Eq,Typeable,Data)

defaultConfig :: Config
defaultConfig = Config {
  repositoryPath      = "wikidata",
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
  recaptchaPrivateKey = ""
  }

type SessionKey = Integer

data SessionData = SessionData {
  sessionUser :: String
} deriving (Read,Show,Eq,Typeable,Data)

data Sessions a = Sessions {unsession::M.Map SessionKey a}
  deriving (Read,Show,Eq,Typeable,Data)

-- Password salt hashedPassword
data Password = Password { pSalt :: String, pHashed :: String }
  deriving (Read,Show,Eq,Typeable,Data)

data User = User {
  uUsername :: String,
  uPassword :: Password,
  uEmail    :: String
} deriving (Show,Read,Typeable,Data)

data AppState = AppState {
  sessions :: Sessions SessionData,
  users    :: M.Map String User,
  config   :: Config
} deriving (Show,Read,Typeable,Data)

instance Version SessionData
instance Version (Sessions a)
instance Version Config

$(deriveSerialize ''SessionData)
$(deriveSerialize ''Sessions)
$(deriveSerialize ''Config)

instance Version AppState
instance Version User
instance Version Password

$(deriveSerialize ''Password)
$(deriveSerialize ''User)
$(deriveSerialize ''AppState)

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState {sessions = (Sessions M.empty), users = M.empty, config = defaultConfig}

askUsers :: MonadReader AppState m => m (M.Map String User)
askUsers = return . users =<< ask

askSessions::MonadReader AppState m => m (Sessions SessionData)
askSessions = return . sessions =<< ask

setUsers :: MonadState AppState m => M.Map String User -> m ()
setUsers newusers = modify $ \s -> s {users = newusers}

modUsers :: MonadState AppState m => (M.Map String User -> M.Map String User) -> m ()
modUsers f = modify $ \s -> s {users = f $ users s}

modSessions :: MonadState AppState m => (Sessions SessionData -> Sessions SessionData) -> m ()
modSessions f = modify $ \s -> s {sessions = f $ sessions s}

isUser :: MonadReader AppState m => String -> m Bool
isUser name = liftM (M.member name) askUsers

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

addUser :: MonadState AppState m => String -> User -> m ()
addUser name = modUsers . M.insert name

delUser :: MonadState AppState m => String -> m ()
delUser = modUsers . M.delete

authUser :: MonadReader AppState m => String -> String -> m Bool
authUser name pass = do
  users' <- askUsers
  case M.lookup name users' of
       Just u  -> do
         let salt = pSalt $ uPassword u
         let hashed = pHashed $ uPassword u
         return $ hashed == hashPassword salt pass
       Nothing -> return False 

listUsers :: MonadReader AppState m => m [String]
listUsers = liftM M.keys askUsers

numUsers ::  MonadReader AppState m => m Int
numUsers = liftM length listUsers

isSession :: MonadReader AppState m => SessionKey -> m Bool
isSession key = liftM (M.member key . unsession) askSessions

setSession :: (MonadState AppState m) => SessionKey -> SessionData -> m ()
setSession key u = do
  modSessions $ Sessions . M.insert key u . unsession
  return ()

newSession :: (MonadState AppState (Ev (t GHC.Conc.STM)), MonadTrans t, Monad (t GHC.Conc.STM)) =>
              SessionData -> Ev (t GHC.Conc.STM) SessionKey
newSession u = do
  key <- getRandom
  setSession key u
  return key

delSession :: (MonadState AppState m) => SessionKey -> m ()
delSession key = do
  modSessions $ Sessions . M.delete key . unsession
  return ()

getSession::SessionKey -> Query AppState (Maybe SessionData)
getSession key = liftM (M.lookup key . unsession) askSessions

getConfig :: Query AppState Config
getConfig = return . config =<< ask

setConfig :: MonadState AppState m => Config ->  m ()
setConfig conf = modify $ \s -> s {config = conf}

numSessions:: Proxy AppState -> Query AppState Int
numSessions = proxyQuery $ liftM (M.size . unsession) askSessions

$(mkMethods ''AppState ['askUsers, 'setUsers, 'addUser, 'delUser, 'authUser, 'isUser, 'listUsers, 'numUsers,
             'isSession, 'setSession, 'getSession, 'newSession, 'delSession, 'numSessions,
             'setConfig, 'getConfig])

