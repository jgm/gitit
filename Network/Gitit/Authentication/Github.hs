{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.Gitit.Authentication.Github (loginGithubUser, getGithubUser, GithubCallbackPars) where

import Network.OAuth.OAuth2

import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad (liftM, mplus, mzero)
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative
import Data.Char (chr)

loginGithubUser :: OAuth2 -> Handler
loginGithubUser githubKey = do
  let state = "testGithubApi"
  let scopes = "user:email"
  let url = authorizationUrl githubKey `appendQueryParam` [("state", state), ("scope", scopes)]
  seeOther (BS.unpack url) $ toResponse ("redirecting to github" :: String)

getGithubUser :: OAuth2                  -- ^ Oauth2 configuration (client secret)
              -> GithubCallbackPars      -- ^ Authentication code gained after authorization
              -> IO (Either String User) -- ^ user email and name (password 'none')
getGithubUser githubKey githubCallbackPars = do
  let (Just code) = rCode githubCallbackPars
  let setting = tlsManagerSettings
  mgr <- newManager setting
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
  closeManager mgr
  mUser

data GithubCallbackPars = GithubCallbackPars { rCode :: Maybe String }
                          deriving Show

instance FromData GithubCallbackPars where
    fromData = do
         vcode <- liftM Just (look "code") `mplus` return Nothing
         return GithubCallbackPars {rCode = vcode }

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
