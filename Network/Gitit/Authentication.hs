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

module Network.Gitit.Authentication where

import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Server
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( password )
import System.Process (readProcessWithExitCode)
import Control.Monad (unless, liftM)
import Control.Monad.Trans (MonadIO(), liftIO)
import System.Exit
import System.Log.Logger (logM, Priority(..))
import Data.Char (isAlphaNum, isAlpha)
import Text.Pandoc.Shared (substitute)
import Data.Maybe (isJust, fromJust)
import Network.URL
import Network.BSD (getHostName)
import qualified Text.StringTemplate as T

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
              [ label << "Username: "
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
sharedForm mbUser = do
  cfg <- getConfig
  let accessQ = case accessQuestion cfg of
                      Nothing          -> noHtml
                      Just (prompt, _) -> label << prompt +++ br +++
                                          X.password "accessCode" ! [size "15", intAttr "tabindex" 1]
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
                                    textfield "username" ! [size "20", intAttr "tabindex" 2] +++ br
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
            , textfield "email" ! [size "20", intAttr "tabindex" 3, value (initField uEmail)], br
            , textfield "full_name_1" ! [size "20", theclass "req"]
            , label << ("Password (at least 6 characters," ++
                        " including at least one non-letter):")
            , br
            , X.password "password" ! [size "20", intAttr "tabindex" 4]
            , stringToHtml " "
            , br
            , label << "Confirm Password:"
            , br
            , X.password "password2" ! [size "20", intAttr "tabindex" 5]
            , stringToHtml " "
            , br
            , captcha
            , submitField ! [intAttr "tabindex" 6]]


sharedValidation :: ValidationType
                 -> Params
                 -> GititServerPart (Either [String] (String,String,String))
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
  peer <- liftM (fst . rqPeer) askRq
  captchaResult <-
    if useRecaptcha cfg
       then if null (recaptchaChallengeField recaptcha) ||
                 null (recaptchaResponseField recaptcha)
               -- no need to bother captcha.net in this case
               then return $ Left "missing-challenge-or-response"
               else liftIO $ do
                      mbIPaddr <- lookupIPAddr peer
                      let ipaddr = case mbIPaddr of
                                        Just ip -> ip
                                        Nothing -> error $
                                          "Could not find ip address for " ++
                                          peer
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
loginForm :: GititServerPart Html
loginForm = do
  cfg <- getConfig
  base' <- getWikiBase
  return $ gui (base' ++ "/_login") ! [identifier "loginForm"] <<
    fieldset <<
      [ label << "Username "
      , textfield "username" ! [size "15", intAttr "tabindex" 1]
      , stringToHtml " "
      , label << "Password "
      , X.password "password" ! [size "15", intAttr "tabindex" 2]
      , stringToHtml " "
      , submit "login" "Login" ! [intAttr "tabindex" 3]
      ] +++
    p << [ stringToHtml "If you do not have an account, "
         , anchor ! [href $ base' ++ "/_register"] << "click here to get one."
         ] +++
    if null (mailCommand cfg)
       then noHtml
       else p << [ stringToHtml "If you forgot your password, "
                 , anchor ! [href $ base' ++ "/_resetPassword"] <<
                     "click here to get a new one."
                 ]

loginUserForm :: Handler
loginUserForm = withData $ \params -> do
  cfg <- getConfig
  referer <- getReferer
  case authenticationMethod cfg of
       FormAuth     -> addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" referer) >>
                       loginUserForm' params
       HTTPAuth     -> error "You must be logged in through HTTP authentication."
       CustomAuth _ -> error "You must be logged in through custom authentication."

loginUserForm' :: Params -> Handler
loginUserForm' _ =
  loginForm >>= formattedPage defaultPageLayout{
                                pgShowPageTools = False,
                                pgTabs = [],
                                pgTitle = "Login"
                                }

loginUser :: Params -> Handler
loginUser params = do
  let uname = pUsername params
  let pword = pPassword params
  let destination = pDestination params
  allowed <- authUser uname pword
  base' <- getWikiBase
  if allowed
    then do
      key <- newSession (SessionData uname)
      addCookie sessionTime (mkCookie "sid" (show key))
      -- remove unneeded destination cookie
      addCookie 0 (mkCookie "destination" base')
      seeOther destination $ toResponse $ p << ("Welcome, " ++ uname)
    else
      loginUserForm'
         params{ pMessages = "Authentication failed." : pMessages params }

logoutUser :: Params -> Handler
logoutUser params = do
  let key = pSessionKey params
  referer <- getReferer
  let destination = substitute " " "%20" referer
  case key of
       Just k  -> do
         delSession k
         -- make cookie expire immediately, effectively deleting it
         addCookie 0 (mkCookie "sid" "-1")
       Nothing -> return ()
  seeOther destination $ toResponse "You have been logged out."

registerUserForm :: Params -> Handler
registerUserForm params = do
  referer <- getReferer
  let destination = case pDestination params of
                          ""  -> referer
                          x   -> x   -- came from login page, preserve prev dest
  addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" destination)
  registerForm >>=
    formattedPage defaultPageLayout{
                    pgShowPageTools = False,
                    pgTabs = [],
                    pgTitle = "Register for an account"
                    }

