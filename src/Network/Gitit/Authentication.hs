{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
                                    , githubAuthHandlers) where

import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Server
import Network.Gitit.Util
import Network.Gitit.Authentication.Github
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import System.Process (readProcessWithExitCode)
import Control.Monad (unless, liftM)
import Control.Monad.Trans (liftIO)
import System.Exit
import System.Log.Logger (logM, Priority(..))
import Data.Char (isAlphaNum, isAlpha)
import qualified Data.Map as M
import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Network.URL (exportURL, add_param, importURL)
import Network.BSD (getHostName)
import qualified Text.StringTemplate as T
import Network.HTTP (urlEncodeVars)
import Codec.Binary.UTF8.String (encodeString)
import Text.Blaze.Html.Renderer.String as Blaze ( renderHtml )
import Text.Blaze.Html5 hiding (i, search, u, s, contents, source, html, title, map)
import qualified Text.Blaze.Html5 as Html5 hiding (search)
import qualified Text.Blaze.Html5.Attributes as Html5.Attr hiding (dir, span)
import Text.Blaze.Html5.Attributes
import Data.String (IsString(fromString))
import qualified Text.XHtml as XHTML
import Data.ByteString.UTF8 (toString)

-- | Replace each occurrence of one sublist in a list with another.
--   Vendored in from pandoc 2.11.4 as 2.12 removed this function.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ xs = xs
substitute target' replacement lst@(x:xs) =
    case stripPrefix target' lst of
      Just lst' -> replacement ++ substitute target' replacement lst'
      Nothing   -> x : substitute target' replacement xs

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


gui :: AttributeValue -> Html -> Html
gui act = Html5.form ! Html5.Attr.action act ! Html5.Attr.method "post"


textfieldInput :: AttributeValue -> AttributeValue -> Html
textfieldInput nameAndId val = input ! type_ "text" ! Html5.Attr.id nameAndId ! name nameAndId ! value val
textfieldInput' :: AttributeValue -> Html
textfieldInput' nameAndId = input ! type_ "text" ! Html5.Attr.id nameAndId ! name nameAndId
passwordInput :: AttributeValue -> Html
passwordInput nameAndId = input ! type_ "password" ! Html5.Attr.id nameAndId ! name nameAndId
submitInput :: AttributeValue -> AttributeValue -> Html
submitInput nameAndId val = input ! type_ "submit" ! Html5.Attr.id nameAndId ! name nameAndId ! value val

intTabindex :: Int -> Attribute
intTabindex i = Html5.Attr.tabindex (fromString $ show i)

resetPasswordRequestForm :: Params -> Handler
resetPasswordRequestForm _ = do
  let passwordForm = gui "" ! Html5.Attr.id "resetPassword" $ fieldset $ mconcat
              [ Html5.label ! Html5.Attr.for "username" $ "Username: "
              , textfieldInput' "username" ! size "20" ! intTabindex 1
              , " "
              , submitInput "resetPassword" "Reset Password" ! intTabindex 2]
  cfg <- getConfig
  let contents = if null (mailCommand cfg)
                    then p $ "Sorry, password reset not available."
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
            p $ mconcat
                 [ "An email has been sent to "
                 , strong $ fromString . uEmail $ fromJust mbUser
                 , br
                 , "Please click on the enclosed link to reset your password."
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
                     (False, _)     -> ["User " ++
                       renderHtml (fromString uname) ++
                       " is not known"]
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
            Just _ -> mempty
            Nothing -> case accessQuestion cfg of
                      Nothing          -> mempty
                      Just (prompt, _) -> mconcat
                        [ Html5.label ! Html5.Attr.for "accessCode" $ fromString prompt
                        , br
                        , passwordInput "accessCode" ! size "15" ! intTabindex 1
                        , br
                        ]
  let captcha = if useRecaptcha cfg
                   then captchaFields (recaptchaPublicKey cfg) Nothing
                   else mempty
  let initField field = case mbUser of
                      Nothing    -> ""
                      Just user  -> field user
  let userNameField = case mbUser of
                      Nothing    -> mconcat
                        [ Html5.label ! Html5.Attr.for "username" $ "Username (at least 3 letters or digits):"
                        , br
                        , textfieldInput' "username" ! size "20" ! intTabindex 2
                        , br
                        ]
                      Just user  -> Html5.label ! Html5.Attr.for "username" $
                                    (fromString $ "Username (cannot be changed): " ++ uUsername user)
                                    <> br
  let submitField = case mbUser of
                      Nothing    -> submitInput "register" "Register"
                      Just _     -> submitInput "resetPassword" "Reset Password"

  return $ gui "" ! Html5.Attr.id "loginForm" $ fieldset $ mconcat
            [ accessQ
            , userNameField
            , Html5.label ! Html5.Attr.for "email" $ "Email (optional, will not be displayed on the Wiki):"
            , br
            , textfieldInput "email" (fromString $ initField uEmail) ! size "20" ! intTabindex 3
            , br ! class_ "req"
            , textfieldInput' "full_name_1" ! size "20" ! class_ "req"
            , br
            , Html5.label ! Html5.Attr.for "password"
                    $ fromString ("Password (at least 6 characters," ++
                        " including at least one non-letter):")
            , br
            , passwordInput "password" ! size "20" ! intTabindex 4
            , " "
            , br
            , Html5.label ! Html5.Attr.for "password2" $ "Confirm Password:"
            , br
            , passwordInput "password2" ! size "20" ! intTabindex 5
            , " "
            , br
            -- Workaround, as ReCaptcha does not work with BlazeHtml
            , preEscapedToHtml (XHTML.renderHtmlFragment captcha)
            , textfieldInput "destination" (fromString dest) ! Html5.Attr.style "display: none;"
            , submitField ! intTabindex 6
            ]


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
         "Username must be at least 3 characters, all letters or digits.")
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
  return $ gui (fromString $ base' ++ "/_login") ! Html5.Attr.id "loginForm" $
    (fieldset $ mconcat
      [ Html5.label ! Html5.Attr.for "username" $ "Username "
      , textfieldInput' "username" ! size "15" ! intTabindex 1
      , " "
      , Html5.label ! Html5.Attr.for "password" $ "Password "
      , passwordInput "password" ! size "15" ! intTabindex 2
      , " "
      , textfieldInput "destination" (fromString dest) ! Html5.Attr.style "display: none;"
      , submitInput "login" "Login" ! intTabindex 3
      ]) <>
    (if disableRegistration cfg
       then mempty
       else p $ mconcat
                 [ "If you do not have an account, "
                 , a ! href (fromString $ base' ++ "/_register?" ++
                     urlEncodeVars [("destination", encodeString dest)]) $ "click here to get one."
                 ]) <>
    (if null (mailCommand cfg)
       then mempty
       else p $ mconcat
                 [ "If you forgot your password, "
                 , a ! href (fromString $ base' ++ "/_resetPassword") $
                     "click here to get a new one."
                 ])

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
      key <- newSession (sessionData uname)
      addCookie (MaxAge $ sessionTimeout cfg) (mkSessionCookie key)
      seeOther (encUrl destination) $ toResponse $ p $ (fromString $ "Welcome, " ++ uname)
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
  seeOther (encUrl dest) $ toResponse ("You have been logged out." :: String)

registerUserForm :: Handler
registerUserForm = registerForm >>=
    formattedPage defaultPageLayout{
                    pgShowPageTools = False,
                    pgTabs = [],
                    pgTitle = "Register for an account"
                    }

regAuthHandlers :: [Handler]
regAuthHandlers =
  [ Network.Gitit.Server.dir "_register"  $ Network.Gitit.Server.method GET >> registerUserForm
  , Network.Gitit.Server.dir "_register"  $ Network.Gitit.Server.method POST >> withData registerUser
  ]

formAuthHandlers :: Bool -> [Handler]
formAuthHandlers disableReg =
  (if disableReg
    then []
    else regAuthHandlers) ++
  [ Network.Gitit.Server.dir "_login"     $ Network.Gitit.Server.method GET  >> loginUserForm
  , Network.Gitit.Server.dir "_login"     $ Network.Gitit.Server.method POST >> withData loginUser
  , Network.Gitit.Server.dir "_logout"    $ Network.Gitit.Server.method GET  >> withData logoutUser
  , Network.Gitit.Server.dir "_resetPassword"   $ Network.Gitit.Server.method GET  >> withData resetPasswordRequestForm
  , Network.Gitit.Server.dir "_resetPassword"   $ Network.Gitit.Server.method POST >> withData resetPasswordRequest
  , Network.Gitit.Server.dir "_doResetPassword" $ Network.Gitit.Server.method GET  >> withData resetPassword
  , Network.Gitit.Server.dir "_doResetPassword" $ Network.Gitit.Server.method POST >> withData doResetPassword
  , Network.Gitit.Server.dir "_user" currentUser
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
  [ Network.Gitit.Server.dir "_logout" logoutUserHTTP
  , Network.Gitit.Server.dir "_login"  $ withData loginUserHTTP
  , Network.Gitit.Server.dir "_user" currentUser ]

oauthGithubCallback :: GithubConfig
                   -> GithubCallbackPars                  -- ^ Authentication code gained after authorization
                   -> Handler
oauthGithubCallback ghConfig githubCallbackPars =
  withData $ \(sk :: Maybe SessionKey) ->
      do
        mbSd <- maybe (return Nothing) getSession sk
        let mbGititState = mbSd >>= sessionGithubData
            githubData = fromMaybe (error "No Github state found in session (is it the same domain?)") mbGititState
            gititState = sessionGithubState githubData
            destination = sessionGithubDestination githubData
        mUser <- getGithubUser ghConfig githubCallbackPars gititState
        base' <- getWikiBase
        case mUser of
          Right user -> do
                     let userEmail = uEmail user
                     updateGititState $ \s -> s { users = M.insert userEmail user (users s) }
                     addUser (uUsername user) user
                     key <- newSession (sessionData userEmail)
                     cfg <- getConfig
                     addCookie (MaxAge $ sessionTimeout cfg) (mkSessionCookie key)
                     seeOther (encUrl destination) $ toResponse ()
          Left err -> do
              liftIO $ logM "gitit" WARNING $ "Login Failed: " ++ ghUserMessage err ++ maybe "" (". Github response" ++) (ghDetails err)
              cfg <- getConfig
              let destination'
                    | requireAuthentication cfg >= ForRead = base' ++ "/_loginFailure"
                    | otherwise                            = destination
              let url = destination' ++ "?message=" ++ ghUserMessage err
              seeOther (encUrl url) $ toResponse ()

githubAuthHandlers :: GithubConfig
                   -> [Handler]
githubAuthHandlers ghConfig =
  [ Network.Gitit.Server.dir "_logout" $ withData logoutUser
  , Network.Gitit.Server.dir "_login" $ withData $ loginGithubUser $ oAuth2 ghConfig
  , Network.Gitit.Server.dir "_loginFailure" $ githubLoginFailure
  , Network.Gitit.Server.dir "_githubCallback" $ withData $ oauthGithubCallback ghConfig
  , Network.Gitit.Server.dir "_user" currentUser ]

githubLoginFailure :: Handler
githubLoginFailure = withData $ \params ->
  formattedPage (pageLayout (pMessages params)) mempty >>= forbidden
  where
    pageLayout msgs =
      defaultPageLayout{ pgShowPageTools = False,
                         pgTabs = [],
                         pgTitle = "Login failure",
                         pgMessages = msgs
                       }

-- | Returns username of logged in user or null string if nobody logged in.
currentUser :: Handler
currentUser = do
  req <- askRq
  ok $ toResponse $ maybe "" toString (getHeader "REMOTE_USER" req)
