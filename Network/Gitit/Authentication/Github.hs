{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.Gitit.Authentication.Github ( loginGithubUser
                                           , getGithubUser
                                           , GithubCallbackPars) where

import Network.OAuth.OAuth2

import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Control.Monad (liftM, mplus, mzero)
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.Char (chr)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

loginGithubUser :: OAuth2 -> Handler
loginGithubUser githubKey = do
  state <- liftIO $ fmap toString nextRandom
  key <- newSession (sessionDataGithubState state)
  cfg <- getConfig
  addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
  let scopes = "user:email"
  let url = authorizationUrl githubKey `appendQueryParam` [("state", BS.pack state), ("scope", scopes)]
  seeOther (BS.unpack url) $ toResponse ("redirecting to github" :: String)

getGithubUser :: OAuth2                  -- ^ Oauth2 configuration (client secret)
              -> GithubCallbackPars      -- ^ Authentication code gained after authorization
              -> String                  -- ^ Github state, we expect the state we sent in loginGithubUser
              -> GititServerPart (Either String User) -- ^ user email and name (password 'none')
getGithubUser githubKey githubCallbackPars githubState =
  withManagerSettings tlsManagerSettings getUserInternal
  where
    getUserInternal mgr = liftIO $ do
      let (Just state) = rState githubCallbackPars
      if state == githubState
        then do
          let (Just code) = rCode githubCallbackPars
          token <- fetchAccessToken mgr githubKey (sToBS code)
          let mUser = case token of
                             Right at -> do
                                        uinfo <- userInfo mgr at
                                        minfo <- mailInfo mgr at
                                        case (uinfo, minfo) of
                                          (Right githubUser, Right githubUserMail) -> do
                                                              user <- mkUser (unpack $ gname githubUser)
                                                                      (unpack $ email $ head githubUserMail)
                                                                      "none"
                                                              return $ Right user
                                          (Left err, _) -> return $ Left $ lbsToStr err
                                          (_, Left err) -> return $ Left $ lbsToStr err
                             Left err ->
                                 return $ Left $  "no access token found yet: " ++ lbsToStr  err
          mUser
        else
          return $ Left $ "returned state: " ++ state ++ ", expected state: " ++  githubState

data GithubCallbackPars = GithubCallbackPars { rCode :: Maybe String
                                             , rState :: Maybe String }
                          deriving Show

instance FromData GithubCallbackPars where
    fromData = do
         vCode <- liftM Just (look "code") `mplus` return Nothing
         vState <- liftM Just (look "state") `mplus` return Nothing
         return GithubCallbackPars {rCode = vCode, rState = vState}

userInfo :: Manager -> AccessToken -> IO (OAuth2Result GithubUser)
userInfo mgr token = authGetJSON mgr token "https://api.github.com/user"

mailInfo :: Manager -> AccessToken -> IO (OAuth2Result [GithubUserMail])
mailInfo mgr token = authGetJSON mgr token "https://api.github.com/user/emails"

data GithubUser = GithubUser { gid   :: Integer
                             , gname :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "id"
                           <*> o .: "name"
    parseJSON _ = mzero

data GithubUserMail = GithubUserMail { email :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUserMail where
    parseJSON (Object o) = GithubUserMail
                           <$> o .: "email"
    parseJSON _ = mzero

sToBS :: String -> BS.ByteString
sToBS = encodeUtf8 . pack

lbsToStr :: BSL.ByteString -> String
lbsToStr = map (chr . fromEnum) . BSL.unpack
