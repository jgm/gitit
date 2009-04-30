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

module Gitit.Authentication where

import Gitit.State
import Gitit.Framework
import Gitit.Layout
import Gitit.Server
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( password )
import System.Process (readProcessWithExitCode)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO(), liftIO)
import System.IO
import System.Exit
import System.Log.Logger (logM, Priority(..))
import qualified Data.Map as M
import Data.Char (isAlphaNum, isAlpha)
import Text.Pandoc.Shared (substitute)
import Data.Maybe (isJust, fromMaybe, fromJust)
import Network.URL
import Network.BSD (getHostName)
import qualified Text.StringTemplate as T

data ValidationType = Register
                    | ResetPassword
                    deriving (Show,Read)

registerUser :: String -> Params -> Web Response
registerUser _ params = do
  result <- sharedValidation Register params
  case result of
    Left errors -> registerForm >>=
          formattedPage defaultPageLayout{
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Register for an account"
                          }
                        "_register" (params { pMessages = errors })
    Right (uname, email, pword) -> do
       user <- liftIO $ mkUser uname email pword
       addUser uname user
       loginUser "/" params{
                       pUsername = uname,
                       pPassword = pword,
                       pEmail = email }

resetPasswordRequestForm :: String -> Params -> Web Response
resetPasswordRequestForm _ params = do
  let passwordForm = gui "" ! [identifier "resetPassword"] << fieldset <<
              [ label << "Username: "
              , textfield "username" ! [size "20"], stringToHtml " "
              , submit "resetPassword" "Reset Password" ]
  cfg <- getConfig
  let contents = if null (mailCommand cfg)
                    then p << "Sorry, password reset not available."
                    else passwordForm
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Reset your password" }
                "_resetPassword" params contents

resetPasswordRequest :: String -> Params -> Web Response
resetPasswordRequest _ params = do
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
      liftIO $ sendReregisterEmail (fromJust mbUser)
      formattedPage defaultPageLayout{
                      pgShowPageTools = False,
                      pgTabs = [],
                      pgTitle = "Resetting your password"
                      }
                    "_resetPassword"  params response
    else registerForm >>=
         formattedPage defaultPageLayout{
                         pgShowPageTools = False,
                         pgTabs = [],
                         pgTitle = "Register for an account"
                         }
                       "_register" params{ pMessages = errors }

resetLink :: User -> String
resetLink user = exportURL $  foldl add_param
                 (fromJust . importURL $ "/_doResetPassword")
                 [("username", uUsername user),
                  ("reset_code", take 20 (pHashed (uPassword user)))]

sendReregisterEmail :: User -> IO ()
sendReregisterEmail user = do
  cfg <- getConfig
  hostname <- getHostName
  let messageTemplate = T.newSTMP $ resetPasswordMessage cfg
  let filledTemplate = T.render .
                       T.setAttribute "username" (uUsername user) .
                       T.setAttribute "useremail" (uEmail user) .
                       T.setAttribute "hostname" hostname .
                       T.setAttribute "port" (show $ portNumber cfg) .
                       T.setAttribute "resetlink" (resetLink user) $
                       messageTemplate
  let (mailcommand:args) = words $ substitute "%s" (uEmail user)
                                   (mailCommand cfg)
  (exitCode, _pOut, pErr) <- readProcessWithExitCode mailcommand args
                               filledTemplate
  logM "gitit" WARNING $ "Sent reset password email to " ++ uUsername user ++
                         " at " ++ uEmail user
  unless (exitCode == ExitSuccess) $
    logM "gitit" WARNING $ mailcommand ++ " failed. " ++ pErr

resetPassword :: String -> Params -> Web Response
resetPassword _ params = do
  users' <- queryAppState users
  let uname = pUsername params
  let user = M.lookup uname users'
  let knownUser = isJust user
  let resetCodeMatches = take 20 (pHashed (uPassword (fromJust user))) ==
                           pResetCode params
  let errors = if knownUser && resetCodeMatches then [] else
                  if knownUser
                     then "Your reset code is invalid, sorry"
                     else "User " ++ uname ++ " is not known here"
  if null errors
     then resetPasswordForm user >>=
          formattedPage defaultPageLayout{
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Reset your registration info"
                          }
                        "_doResetPassword" params
     else registerForm >>=
          formattedPage defaultPageLayout{
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Register for an account"
                          }
                        "_register" params{ pMessages = [errors] }

doResetPassword :: String -> Params -> Web Response
doResetPassword _ params = do
  let uname = pUsername params
  users' <- queryAppState users
  let mbUser = M.lookup uname users'
  result <- sharedValidation ResetPassword params
  case result of
    Left errors ->
      resetPasswordForm mbUser >>=
          formattedPage defaultPageLayout{
                          pgShowPageTools = False,
                          pgTabs = [],
                          pgTitle = "Reset your registration info"
                          }
                        "_register" params{ pMessages = errors }
    Right (uname', email, pword) -> do
       user <- liftIO $ mkUser uname' email pword
       adjustUser uname' user
       liftIO $ logM "gitit" WARNING $
            "Successfully reset password and email for " ++ uUsername user
       loginUser "/" params{ pUsername = uname',
                             pPassword = pword,
                             pEmail = email }

registerForm :: Web Html
registerForm = sharedForm Nothing

resetPasswordForm :: Maybe User -> Web Html
resetPasswordForm = sharedForm  -- synonym for now

sharedForm :: Maybe User -> Web Html
sharedForm mbUser = do
  cfg <- getConfig
  let accessQ = case accessQuestion cfg of
                      Nothing          -> noHtml
                      Just (prompt, _) -> label << prompt +++ br +++
                                          X.password "accessCode" ! [size "15"]
                                          +++ br
  let captcha = if useRecaptcha cfg
                   then captchaFields (recaptchaPublicKey cfg) Nothing
                   else noHtml
  let initField field = case mbUser of
                      Nothing    -> ""
                      Just user  -> field user
  let userNameField = case mbUser of
                      Nothing    -> label <<
                                     "Username (at least 3 letters or digits):"
                                    +++ br +++
                                    textfield "username" ! [size "20"] +++ br
                      Just user  -> label << ("Username (cannot be changed): "
                                               ++ uUsername user) +++ br
  let submitField = case mbUser of
                      Nothing    -> submit "register" "Register"
                      Just _     -> submit "resetPassword" "Reset Password"

  return $ gui "" ! [identifier "loginForm"] << fieldset <<
            [ accessQ
            , userNameField
            , label << "Email (optional, will not be displayed on the Wiki):"
            , br
            , textfield "email" ! [size "20", value (initField uEmail)], br
            , textfield "full_name_1" ! [size "20", theclass "req"]
            , label << ("Password (at least 6 characters," ++
                        " including at least one non-letter):")
            , br
            , X.password "password" ! [size "20"]
            , stringToHtml " "
            , br
            , label << "Confirm Password:"
            , br
            , X.password "password2" ! [size "20"]
            , stringToHtml " "
            , br
            , captcha
            , submitField ]


sharedValidation :: MonadIO m => ValidationType -> Params
                 -> m (Either [String] (String,String,String))
sharedValidation validationType params = do
  let isValidUsername u = length u >= 3 && all isAlphaNum u
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
  let isValidAccessCode = case accessQuestion cfg of
        Nothing           -> True
        Just (_, answers) -> accessCode `elem` answers
  let isValidEmail e = length (filter (=='@') e) == 1
  captchaResult <-
    if useRecaptcha cfg
       then if null (recaptchaChallengeField recaptcha) ||
                 null (recaptchaResponseField recaptcha)
               -- no need to bother captcha.net in this case
               then return $ Left "missing-challenge-or-response"
               else liftIO $ do
                      mbIPaddr <- lookupIPAddr $ pPeer params
                      let ipaddr = case mbIPaddr of
                                        Just ip -> ip
                                        Nothing -> error $
                                          "Could not find ip address for " ++
                                          pPeer params
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
loginForm :: Web Html
loginForm = do
  cfg <- getConfig
  return $ gui "/_login" ! [identifier "loginForm"] <<
    fieldset <<
      [ label << "Username "
      , textfield "username" ! [size "15"]
      , stringToHtml " "
      , label << "Password "
      , X.password "password" ! [size "15"]
      , stringToHtml " "
      , submit "login" "Login"
      ] +++
    p << [ stringToHtml "If you do not have an account, "
         , anchor ! [href "/_register"] << "click here to get one."
         ] +++
    if null (mailCommand cfg)
       then noHtml
       else p << [ stringToHtml "If you forgot your password, "
                 , anchor ! [href "/_resetPassword"] <<
                     "click here to get a new one."
                 ]

loginUserForm :: String -> Params -> Web Response
loginUserForm page params =
  addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" $
                        fromMaybe "/" $ pReferer params) >>
  loginUserForm' page params

loginUserForm' :: String -> Params -> Web Response
loginUserForm' page params =
  loginForm >>= formattedPage defaultPageLayout{
                                pgShowPageTools = False,
                                pgTabs = [],
                                pgTitle = "Login"
                                }
                              page params

loginUser :: String -> Params -> Web Response
loginUser page params = do
  let uname = pUsername params
  let pword = pPassword params
  let destination = pDestination params
  allowed <- authUser uname pword
  if allowed
    then do
      key <- newSession (SessionData uname)
      addCookie sessionTime (mkCookie "sid" (show key))
      -- remove unneeded destination cookie
      addCookie 0 (mkCookie "destination" "/")
      seeOther destination $ toResponse $ p << ("Welcome, " ++ uname)
    else
      loginUserForm' page
         params{ pMessages = "Authentication failed." : pMessages params }

logoutUser :: String -> Params -> Web Response
logoutUser _ params = do
  let key = pSessionKey params
  let destination = substitute " " "%20" $ fromMaybe "/" $ pReferer params
  case key of
       Just k  -> do
         delSession k
         -- make cookie expire immediately, effectively deleting it
         addCookie 0 (mkCookie "sid" "-1")
       Nothing -> return ()
  seeOther destination $ toResponse "You have been logged out."

registerUserForm :: String -> Params -> Web Response
registerUserForm _ params =
  addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" $
                         fromMaybe "/" $ pReferer params) >>
  registerForm >>=
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Register for an account"
                  }
                "_register" params

