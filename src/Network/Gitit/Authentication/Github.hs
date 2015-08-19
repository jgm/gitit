{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Network.Gitit.Authentication.Github ( loginGithubUser
                                           , getGithubUser
                                           , GithubCallbackPars
                                           , GithubLoginError
                                           , ghUserMessage
                                           , ghDetails) where

import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2
import Control.Monad (liftM, mplus, mzero)
import Data.Maybe
import Data.Aeson
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import qualified Control.Exception as E
import Prelude

loginGithubUser :: OAuth2 -> Handler
loginGithubUser githubKey = do
  state <- liftIO $ fmap toString nextRandom
  key <- newSession (sessionDataGithubState state)
  cfg <- getConfig
  addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
  let usingOrg = isJust $ org $ githubAuth cfg
  let scopes = "user:email" ++ if usingOrg then ",read:org" else ""
  let url = authorizationUrl githubKey `appendQueryParam` [("state", BS.pack state), ("scope", BS.pack scopes)]
  seeOther (BS.unpack url) $ toResponse ("redirecting to github" :: String)

data GithubLoginError = GithubLoginError { ghUserMessage :: String
                                         , ghDetails :: Maybe String
                                         }

getGithubUser :: GithubConfig            -- ^ Oauth2 configuration (client secret)
              -> GithubCallbackPars      -- ^ Authentication code gained after authorization
              -> String                  -- ^ Github state, we expect the state we sent in loginGithubUser
              -> GititServerPart (Either GithubLoginError User) -- ^ user email and name (password 'none')
getGithubUser ghConfig githubCallbackPars githubState =
       withManagerSettings tlsManagerSettings getUserInternal
    where
    getUserInternal mgr =
        liftIO $ do
            let (Just state) = rState githubCallbackPars
            if state == githubState
              then do
                let (Just code) = rCode githubCallbackPars
                ifSuccess
                   "No access token found yet"
                   (fetchAccessToken mgr (oAuth2 ghConfig) (sToBS code))
                   (\at -> ifSuccess
                           "User Authentication failed"
                           (userInfo mgr at)
                           (\githubUser -> ifSuccess
                            ("No email for user " ++ unpack (gLogin githubUser) ++ " returned by Github")
                            (mailInfo mgr at)
                            (\githubUserMail -> do
                                       let gitLogin = gLogin githubUser
                                       user <- mkUser (unpack gitLogin)
                                                   (unpack $ email $ head githubUserMail)
                                                   "none"
                                       let mbOrg = org ghConfig
                                       case mbOrg of
                                             Nothing -> return $ Right user
                                             Just githuborg -> ifSuccess
                                                      ("Membership check of user " ++ unpack gitLogin ++  " to "  ++ unpack githuborg ++ " failed")
                                                      (orgInfo gitLogin githuborg mgr at)
                                                      (\_ -> return $ Right user))))
              else
                return $ Left $
                       GithubLoginError ("The state sent to github is not the same as the state received: " ++ state ++ ", but expected sent state: " ++  githubState)
                                        Nothing
    ifSuccess errMsg failableAction successAction  = E.catch
                                                 (do Right outcome <- failableAction
                                                     successAction outcome)
                                                 (\exception -> liftIO $ return $ Left $
                                                                GithubLoginError errMsg
                                                                                 (Just $ show (exception :: E.SomeException)))

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

orgInfo  :: Text -> Text -> Manager -> AccessToken -> IO (OAuth2Result BSL.ByteString)
orgInfo gitLogin githubOrg mgr token = do
  let url  = "https://api.github.com/orgs/" `BS.append` encodeUtf8 githubOrg `BS.append` "/members/" `BS.append` encodeUtf8 gitLogin
  authGetBS mgr token url

data GithubUser = GithubUser { gLogin :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "login"
    parseJSON _ = mzero

data GithubUserMail = GithubUserMail { email :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUserMail where
    parseJSON (Object o) = GithubUserMail
                           <$> o .: "email"
    parseJSON _ = mzero

sToBS :: String -> BS.ByteString
sToBS = encodeUtf8 . pack
