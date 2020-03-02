{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}

module Network.Gitit.Authentication.Github ( loginGithubUser
                                           , getGithubUser
                                           , GithubCallbackPars
                                           , GithubLoginError
                                           , ghUserMessage
                                           , ghDetails) where

import Network.Gitit.Types
import Network.Gitit.Server
import Network.Gitit.State
import Network.Gitit.Util
import Network.Gitit.Framework
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified URI.ByteString as URI
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest as OA
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

loginGithubUser :: OAuth2 -> Params -> Handler
loginGithubUser githubKey params = do
  state <- liftIO $ fmap toString nextRandom
  base' <- getWikiBase
  let destination = pDestination params `orIfNull` (base' ++ "/")
  key <- newSession $ sessionDataGithubStateUrl state destination
  cfg <- getConfig
  addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
  let usingOrg = isJust $ org $ githubAuth cfg
  let scopes = "user:email" ++ if usingOrg then ",read:org" else ""
  let url = appendQueryParams [("state", BS.pack state), ("scope", BS.pack scopes)] $ authorizationUrl githubKey
  seeOther (BS.unpack (URI.serializeURIRef' url)) $ toResponse ("redirecting to github" :: String)

data GithubLoginError = GithubLoginError { ghUserMessage :: String
                                         , ghDetails :: Maybe String
                                         }

getGithubUser :: GithubConfig            -- ^ Oauth2 configuration (client secret)
              -> GithubCallbackPars      -- ^ Authentication code gained after authorization
              -> String                  -- ^ Github state, we expect the state we sent in loginGithubUser
              -> GititServerPart (Either GithubLoginError User) -- ^ user email and name (password 'none')
getGithubUser ghConfig githubCallbackPars githubState = liftIO $
  newManager tlsManagerSettings >>= getUserInternal
    where
    getUserInternal mgr =
        liftIO $ do
            let (Just state) = rState githubCallbackPars
            if state == githubState
              then do
                let (Just code) = rCode githubCallbackPars
                ifSuccess
                   "No access token found yet"
                   (fetchAccessToken mgr (oAuth2 ghConfig) (ExchangeToken $ pack code))
                   (\at -> ifSuccess
                           "User Authentication failed"
                           (userInfo mgr (accessToken at))
                           (\githubUser -> ifSuccess
                            ("No email for user " ++ unpack (gLogin githubUser) ++ " returned by Github")
                            (mailInfo mgr (accessToken at))
                            (\githubUserMail -> do
                                       let gitLogin = gLogin githubUser
                                       user <- mkUser (unpack gitLogin)
                                                   (unpack $ email $ head (filter primary githubUserMail))
                                                   "none"
                                       let mbOrg = org ghConfig
                                       case mbOrg of
                                             Nothing -> return $ Right user
                                             Just githuborg -> ifSuccess
                                                      ("Membership check failed: the user " ++ unpack gitLogin ++  " is required to be a member of the organization "  ++ unpack githuborg ++ ".")
                                                      (orgInfo gitLogin githuborg mgr (accessToken at))
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

#if MIN_VERSION_hoauth2(1, 9, 0)
userInfo :: Manager -> AccessToken -> IO (Either BSL.ByteString GithubUser)
#else
userInfo :: Manager -> AccessToken -> IO (OAuth2Result OA.Errors GithubUser)
#endif
userInfo mgr token = authGetJSON mgr token $ githubUri "/user"

#if MIN_VERSION_hoauth2(1, 9, 0)
mailInfo :: Manager -> AccessToken -> IO (Either BSL.ByteString [GithubUserMail])
#else
mailInfo :: Manager -> AccessToken -> IO (OAuth2Result OA.Errors [GithubUserMail])
#endif
mailInfo mgr token = authGetJSON mgr token $ githubUri "/user/emails"

#if MIN_VERSION_hoauth2(1, 9, 0)
orgInfo  :: Text -> Text -> Manager -> AccessToken -> IO (Either BSL.ByteString BSL.ByteString)
#else
orgInfo  :: Text -> Text -> Manager -> AccessToken -> IO (OAuth2Result OA.Errors BSL.ByteString)
#endif
orgInfo gitLogin githubOrg mgr token = do
  let url = githubUri $ "/orgs/" `BS.append` encodeUtf8 githubOrg `BS.append` "/members/" `BS.append` encodeUtf8 gitLogin
  authGetBS mgr token url

type UriPath = BS.ByteString

githubUri :: UriPath -> URI.URI
githubUri p = URI.URI { URI.uriScheme    = URI.Scheme "https"
                      , URI.uriAuthority = Just $ URI.Authority Nothing (URI.Host "api.github.com") Nothing
                      , URI.uriPath      = p
                      , URI.uriQuery     = URI.Query []
                      , URI.uriFragment  = Nothing }

data GithubUser = GithubUser { gLogin :: Text
                             } deriving (Show, Eq)

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
                           <$> o .: "login"
    parseJSON _ = mzero

data GithubUserMail = GithubUserMail { email :: Text
                                     , primary :: Bool
                             } deriving (Show, Eq)

instance FromJSON GithubUserMail where
    parseJSON (Object o) = GithubUserMail
                           <$> o .: "email"
                           <*> o .: "primary"
    parseJSON _ = mzero
