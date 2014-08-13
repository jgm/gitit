{-# LANGUAGE ScopedTypeVariables #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>,
                   Henry Laxen <nadine.and.henry@pobox.com>

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

{- Handlers for registering and authenticating users.
-}

module Network.Gitit.Authentication ( loginUserForm
                                    , formAuthHandlers
                                    , httpAuthHandlers
                                    , rpxAuthHandlers
                                    , githubAuthHandlers) where

import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Server
import Network.Gitit.Util
import Network.Gitit.Authentication.Github
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( password )
import System.Process (readProcessWithExitCode)
import Control.Monad (unless, liftM, mplus)
import Control.Monad.Trans (liftIO)
import System.Exit
import System.Log.Logger (logM, Priority(..))
import Data.Char (isAlphaNum, isAlpha)
import qualified Data.Map as M
import Text.Pandoc.Shared (substitute)
import Data.Maybe (isJust, fromJust, isNothing, fromMaybe)
import Network.URL (exportURL, add_param, importURL)
import Network.BSD (getHostName)
import qualified Text.StringTemplate as T
import Network.HTTP (urlEncodeVars, urlDecode, urlEncode)
import Codec.Binary.UTF8.String (encodeString)
import Data.ByteString.UTF8 (toString)
import Network.Gitit.Rpxnow as R
import Network.OAuth.OAuth2

data ValidationType = Register
                    | ResetPassword
                    deriving (Show,Read)

registerUser :: Params -> Handler
registerUser params = do
  result' <- sharedValidation Register params
  case result' of
    Left errors -> registerForm >>=
          formattedPage defaultPageLayout{
                          pgMessages = errors,
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Register for an account"
                          }
    Right (uname, email, pword) -> do
       user <- liftIO $ mkUser uname email pword
       addUser uname user
       loginUser params{ pUsername = uname,
                         pPassword = pword,
                         pEmail = email }

resetPasswordRequestForm :: Params -> Handler
resetPasswordRequestForm _ = do
  let passwordForm = gui "" ! [identifier "resetPassword"] << fieldset <<
              [ label ! [thefor "username"] << "Username: "
              , textfield "username" ! [size "20", intAttr "tabindex" 1], stringToHtml " "
              , submit "resetPassword" "Reset Password" ! [intAttr "tabindex" 2]]
  cfg <- getConfig
  let contents = if null (mailCommand cfg)
                    then p << "Sorry, password reset not available."
                    else passwordForm
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Reset your password" }
                contents

resetPasswordRequest :: Params -> Handler
resetPasswordRequest params = do
  let uname = pUsername params
  mbUser <- getUser uname
  let errors = case mbUser of
        Nothing -> ["Unknown user. Please re-register " ++
                    "or press the Back button to try again."]
        Just u  -> ["Since you did not register with " ++
                   "an email address, we can't reset your password." |
                    null (uEmail u) ]
  if null errors
    then do
      let response =
            p << [ stringToHtml "An email has been sent to "
                 , bold $ stringToHtml . uEmail $ fromJust mbUser
                 , br
                 , stringToHtml
                   "Please click on the enclosed link to reset your password."
                 ]
      sendReregisterEmail (fromJust mbUser)
      formattedPage defaultPageLayout{
                      pgShowPageTools = False,
                      pgTabs = [],
                      pgTitle = "Resetting your password"
                      }
                    response
    else registerForm >>=
         formattedPage defaultPageLayout{
                         pgMessages = errors,
                         pgShowPageTools = False,
                         pgTabs = [],
                         pgTitle = "Register for an account"
                         }

resetLink :: String -> User -> String
resetLink base' user =
  exportURL $  foldl add_param
    (fromJust . importURL $ base' ++ "/_doResetPassword")
    [("username", uUsername user), ("reset_code", take 20 (pHashed (uPassword user)))]

sendReregisterEmail :: User -> GititServerPart ()
sendReregisterEmail user = do
  cfg <- getConfig
  hostname <- liftIO getHostName
  base' <- getWikiBase
  let messageTemplate = T.newSTMP $ resetPasswordMessage cfg
  let filledTemplate = T.render .
                       T.setAttribute "username" (uUsername user) .
                       T.setAttribute "useremail" (uEmail user) .
                       T.setAttribute "hostname" hostname .
                       T.setAttribute "port" (show $ portNumber cfg) .
                       T.setAttribute "resetlink" (resetLink base' user) $
                       messageTemplate
  let (mailcommand:args) = words $ substitute "%s" (uEmail user)
                                   (mailCommand cfg)
  (exitCode, _pOut, pErr) <- liftIO $ readProcessWithExitCode mailcommand args
                                      filledTemplate
  liftIO $ logM "gitit" WARNING $ "Sent reset password email to " ++ uUsername user ++
                         " at " ++ uEmail user
  unless (exitCode == ExitSuccess) $
    liftIO $ logM "gitit" WARNING $ mailcommand ++ " failed. " ++ pErr

validateReset :: Params -> (User -> Handler) -> Handler
validateReset params postValidate = do
  let uname = pUsername params
  user <- getUser uname
  let knownUser = isJust user
  let resetCodeMatches = take 20 (pHashed (uPassword (fromJust user))) ==
                           pResetCode params
  let errors = case (knownUser, resetCodeMatches) of
                     (True, True)   -> []
                     (True, False)  -> ["Your reset code is invalid"]
                     (False, _)     -> ["User " ++ uname ++ " is not known"]
  if null errors
     then postValidate (fromJust user)
     else registerForm >>=
          formattedPage defaultPageLayout{
                          pgMessages = errors,
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Register for an account"
                          }

resetPassword :: Params -> Handler
resetPassword params = validateReset params $ \user ->
  resetPasswordForm (Just user) >>=
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Reset your registration info"
                  }

doResetPassword :: Params -> Handler
doResetPassword params = validateReset params $ \user -> do
  result' <- sharedValidation ResetPassword params
  case result' of
    Left errors ->
      resetPasswordForm (Just user) >>=
          formattedPage defaultPageLayout{
                          pgMessages = errors,
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Reset your registration info"
                          }
    Right (uname, email, pword) -> do
       user' <- liftIO $ mkUser uname email pword
       adjustUser uname user'
       liftIO $ logM "gitit" WARNING $
            "Successfully reset password and email for " ++ uUsername user'
       loginUser params{ pUsername = uname,
                         pPassword = pword,
                         pEmail = email }

registerForm :: GititServerPart Html
registerForm = sharedForm Nothing

resetPasswordForm :: Maybe User -> GititServerPart Html
resetPasswordForm = sharedForm  -- synonym for now

sharedForm :: Maybe User -> GititServerPart Html
sharedForm mbUser = withData $ \params -> do
  cfg <- getConfig
  dest <- case pDestination params of
                ""  -> getReferer
                x   -> return x
  let accessQ = case mbUser of
            Just _ -> noHtml
            Nothing -> case accessQuestion cfg of
                      Nothing          -> noHtml
                      Just (prompt, _) -> label ! [thefor "accessCode"] << prompt +++ br +++
                                          X.password "accessCode" ! [size "15", intAttr "tabindex" 1]
                                          +++ br
  let captcha = if useRecaptcha cfg
                   then captchaFields (recaptchaPublicKey cfg) Nothing
                   else noHtml
  let initField field = case mbUser of
                      Nothing    -> ""
                      Just user  -> field user
  let userNameField = case mbUser of
                      Nothing    -> label ! [thefor "username"] <<
                                     "Username (at least 3 letters or digits):"
                                    +++ br +++
                                    textfield "username" ! [size "20", intAttr "tabindex" 2] +++ br
                      Just user  -> label ! [thefor "username"] <<
                                    ("Username (cannot be changed): " ++ uUsername user)
                                    +++ br
  let submitField = case mbUser of
                      Nothing    -> submit "register" "Register"
                      Just _     -> submit "resetPassword" "Reset Password"

  return $ gui "" ! [identifier "loginForm"] << fieldset <<
            [ accessQ
            , userNameField
            , label ! [thefor "email"] << "Email (optional, will not be displayed on the Wiki):"
            , br
            , textfield "email" ! [size "20", intAttr "tabindex" 3, value (initField uEmail)], br
            , textfield "full_name_1" ! [size "20", theclass "req"]
            , label ! [thefor "password"]
                    << ("Password (at least 6 characters," ++
                        " including at least one non-letter):")
            , br
            , X.password "password" ! [size "20", intAttr "tabindex" 4]
            , stringToHtml " "
            , br
            , label ! [thefor "password2"] << "Confirm Password:"
            , br
            , X.password "password2" ! [size "20", intAttr "tabindex" 5]
            , stringToHtml " "
            , br
            , captcha
            , textfield "destination" ! [thestyle "display: none;", value dest]
            , submitField ! [intAttr "tabindex" 6]]


sharedValidation :: ValidationType
                 -> Params
                 -> GititServerPart (Either [String] (String,String,String))
sharedValidation validationType params = do
  let isValidUsernameChar c = isAlphaNum c || c == ' '
  let isValidUsername u = length u >= 3 && all isValidUsernameChar u
  let isValidPassword pw = length pw >= 6 && not (all isAlpha pw)
  let accessCode = pAccessCode params
  let uname = pUsername params
  let pword = pPassword params
  let pword2 = pPassword2 params
  let email = pEmail params
  let fakeField = pFullName params
  let recaptcha = pRecaptcha params
  taken <- isUser uname
  cfg <- getConfig
  let optionalTests Register =
          [(taken, "Sorry, that username is already taken.")]
      optionalTests ResetPassword = []
  let isValidAccessCode = case validationType of
        ResetPassword -> True
        Register -> case accessQuestion cfg of
            Nothing           -> True
            Just (_, answers) -> accessCode `elem` answers
  let isValidEmail e = length (filter (=='@') e) == 1
  peer <- liftM (fst . rqPeer) askRq
  captchaResult <-
    if useRecaptcha cfg
       then if null (recaptchaChallengeField recaptcha) ||
                 null (recaptchaResponseField recaptcha)
               -- no need to bother captcha.net in this case
               then return $ Left "missing-challenge-or-response"
               else liftIO $ do
                      mbIPaddr <- lookupIPAddr peer
                      let ipaddr = fromMaybe (error $ "Could not find ip address for " ++ peer)
                                   mbIPaddr
                      ipaddr `seq` validateCaptcha (recaptchaPrivateKey cfg)
                              ipaddr (recaptchaChallengeField recaptcha)
                              (recaptchaResponseField recaptcha)
       else return $ Right ()
  let (validCaptcha, captchaError) =
        case captchaResult of
              Right () -> (True, Nothing)
              Left err -> (False, Just err)
  let errors = validate $ optionalTests validationType ++
        [ (not isValidAccessCode, "Incorrect response to access prompt.")
        , (not (isValidUsername uname),
         "Username must be at least 3 charcaters, all letters or digits.")
        , (not (isValidPassword pword),
         "Password must be at least 6 characters, " ++
         "and must contain at least one non-letter.")
        , (not (null email) && not (isValidEmail email),
         "Email address appears invalid.")
        , (pword /= pword2,
        "Password does not match confirmation.")
        , (not validCaptcha,
        "Failed CAPTCHA (" ++ fromJust captchaError ++
        "). Are you really human?")
        , (not (null fakeField), -- fakeField is hidden in CSS (honeypot)
        "You do not seem human enough. If you're sure you are human, " ++
        "try turning off form auto-completion in your browser.")
        ]
  return $ if null errors then Right (uname, email, pword) else Left errors

-- user authentication
loginForm :: String -> GititServerPart Html
loginForm dest = do
  cfg <- getConfig
  base' <- getWikiBase
  return $ gui (base' ++ "/_login") ! [identifier "loginForm"] <<
    fieldset <<
      [ label ! [thefor "username"] << "Username "
      , textfield "username" ! [size "15", intAttr "tabindex" 1]
      , stringToHtml " "
      , label ! [thefor "password"] << "Password "
      , X.password "password" ! [size "15", intAttr "tabindex" 2]
      , stringToHtml " "
      , textfield "destination" ! [thestyle "display: none;", value dest]
      , submit "login" "Login" ! [intAttr "tabindex" 3]
      ] +++
    p << [ stringToHtml "If you do not have an account, "
         , anchor ! [href $ base' ++ "/_register?" ++
           urlEncodeVars [("destination", encodeString dest)]] << "click here to get one."
         ] +++
    if null (mailCommand cfg)
       then noHtml
       else p << [ stringToHtml "If you forgot your password, "
                 , anchor ! [href $ base' ++ "/_resetPassword"] <<
                     "click here to get a new one."
                 ]

loginUserForm :: Handler
loginUserForm = withData $ \params -> do
  dest <- case pDestination params of
                ""  -> getReferer
                x   -> return x
  loginForm dest >>=
    formattedPage defaultPageLayout{ pgShowPageTools = False,
                                     pgTabs = [],
                                     pgTitle = "Login",
                                     pgMessages = pMessages params
                                   }

loginUser :: Params -> Handler
loginUser params = do
  let uname = pUsername params
  let pword = pPassword params
  let destination = pDestination params
  allowed <- authUser uname pword
  cfg <- getConfig
  if allowed
    then do
      key <- newSession (SessionData uname)
      addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
      seeOther (encUrl destination) $ toResponse $ p << ("Welcome, " ++ uname)
    else
      withMessages ["Invalid username or password."] loginUserForm

logoutUser :: Params -> Handler
logoutUser params = do
  let key = pSessionKey params
  dest <- case pDestination params of
                ""  -> getReferer
                x   -> return x
  case key of
       Just k  -> do
         delSession k
         expireCookie "sid"
       Nothing -> return ()
  seeOther (encUrl dest) $ toResponse "You have been logged out."

registerUserForm :: Handler
registerUserForm = registerForm >>=
    formattedPage defaultPageLayout{
                    pgShowPageTools = False,
                    pgTabs = [],
                    pgTitle = "Register for an account"
                    }

formAuthHandlers :: [Handler]
formAuthHandlers =
  [ dir "_register"  $ method GET >> registerUserForm
  , dir "_register"  $ method POST >> withData registerUser
  , dir "_login"     $ method GET  >> loginUserForm
  , dir "_login"     $ method POST >> withData loginUser
  , dir "_logout"    $ method GET  >> withData logoutUser
  , dir "_resetPassword"   $ method GET  >> withData resetPasswordRequestForm
  , dir "_resetPassword"   $ method POST >> withData resetPasswordRequest
  , dir "_doResetPassword" $ method GET  >> withData resetPassword
  , dir "_doResetPassword" $ method POST >> withData doResetPassword
  , dir "_user" currentUser
  ]

loginUserHTTP :: Params -> Handler
loginUserHTTP params = do
  base' <- getWikiBase
  let destination = pDestination params `orIfNull` (base' ++ "/")
  seeOther (encUrl destination) $ toResponse ()

logoutUserHTTP :: Handler
logoutUserHTTP = unauthorized $ toResponse ()  -- will this work?

httpAuthHandlers :: [Handler]
httpAuthHandlers =
  [ dir "_logout" logoutUserHTTP
  , dir "_login"  $ withData loginUserHTTP
  , dir "_user" currentUser ]

oauthGithubCallback :: OAuth2
                   -> GithubCallbackPars                  -- ^ Authentication code gained after authorization
                   -> Handler
oauthGithubCallback githubKey githubCallbackPars = do
    mUser <- liftIO $ getGithubUser githubKey githubCallbackPars
    case mUser of
      Right user -> do
               let userEmail = uEmail user
               updateGititState $ \s -> s { users = M.insert userEmail user (users s) }
               addUser (uUsername user) user
               key <- newSession (SessionData userEmail)
               cfg <- getConfig
               addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
               base' <- getWikiBase
               let destination = base' ++ "/"
               seeOther (encUrl destination) $ toResponse ()
      Left err ->
               error err

githubAuthHandlers :: OAuth2
                   -> [Handler]
githubAuthHandlers githubKey =
  [ dir "_logout" $ withData logoutUser
  , dir "_login" $ loginGithubUser githubKey
  , dir "_githubCallback" $ withData $ oauthGithubCallback githubKey
  , dir "_user" currentUser ]

-- Login using RPX (see RPX development docs at https://rpxnow.com/docs)
loginRPXUser :: RPars  -- ^ The parameters passed by the RPX callback call (after authentication has taken place
             -> Handler
loginRPXUser params = do
  cfg <- getConfig
  ref <- getReferer
  let mtoken = rToken params
  if isNothing mtoken
     then do
       let url = baseUrl cfg ++ "/_login?destination=" ++
                  fromMaybe ref (rDestination params)
       if null (rpxDomain cfg)
          then error "rpx-domain is not set."
          else do
             let rpx = "https://" ++ rpxDomain cfg ++
                       ".rpxnow.com/openid/v2/signin?token_url=" ++
                       urlEncode url
             see rpx
     else do -- We got an answer from RPX, this might also return an exception.
       uid' :: Either String R.Identifier <- liftIO $
                      R.authenticate (rpxKey cfg) $ fromJust mtoken
       uid <- case uid' of
                   Right u -> return u
                   Left err -> error err
       liftIO $ logM "gitit.loginRPXUser" DEBUG $ "uid:" ++ show uid
       -- We need to get an unique identifier for the user
       -- The 'identifier' is always present but can be rather cryptic
       -- The 'verifiedEmail' is also unique and is a more readable choice
       -- so we use it if present.
       let userId = R.userIdentifier uid
       let email  = prop "verifiedEmail" uid
       user <- liftIO $ mkUser (fromMaybe userId email) (fromMaybe "" email) "none"
       updateGititState $ \s -> s { users = M.insert userId user (users s) }
       key <- newSession (SessionData userId)
       addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show key))
       see $ fromJust $ rDestination params
      where
        prop pname info = lookup pname $ R.userData info
        see url = seeOther (encUrl url) $ toResponse noHtml

-- The parameters passed by the RPX callback call.
data RPars = RPars { rToken       :: Maybe String
                   , rDestination :: Maybe String }
                   deriving Show

instance FromData RPars where
     fromData = do
         vtoken <- liftM Just (look "token") `mplus` return Nothing
         vDestination <- liftM (Just . urlDecode) (look "destination") `mplus`
                           return Nothing
         return RPars { rToken = vtoken
                      , rDestination = vDestination }

rpxAuthHandlers :: [Handler]
rpxAuthHandlers =
  [ dir "_logout" $ method GET >> withData logoutUser
  , dir "_login"  $ withData loginRPXUser
  , dir "_user" currentUser ]

-- | Returns username of logged in user or null string if nobody logged in.
currentUser :: Handler
currentUser = do
  req <- askRq
  ok $ toResponse $ maybe "" toString (getHeader "REMOTE_USER" req)


