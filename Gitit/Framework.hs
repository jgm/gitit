{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>
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

{- General framework for defining wiki actions. 
-}

module Gitit.Framework (
                         Handler
                       , Recaptcha(..)
                       , Params(..)
                       , Command(..)
                       , getLoggedInUser
                       , sessionTime
                       , unlessNoEdit
                       , unlessNoDelete
                       , handle
                       , handlePage
                       , handleText
                       , handlePath
                       , withCommand
                       , uriPath
                       , isPage
                       , isDiscussPage
                       , isSourceCode
                       , urlForPage
                       , pathForPage
                       , withCommands
                       , getMimeTypeForExtension
                       , ifLoggedIn
                       , validate
                       )
where
import Gitit.Server
import Gitit.State
import Text.Pandoc.Shared (substitute)
import Control.Monad.Reader (mplus)
import Data.Char (toLower)
import Data.DateTime
import Control.Monad.Trans (MonadIO)
import Control.Monad (msum, mzero)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intersect, intercalate, isSuffixOf)
import System.FilePath ((<.>), takeExtension)
import Codec.Binary.UTF8.String (decodeString, encodeString)
import Text.Highlighting.Kate
import Network.HTTP (urlEncode)

type Handler = ServerPart Response

data Recaptcha = Recaptcha {
    recaptchaChallengeField :: String
  , recaptchaResponseField  :: String
  } deriving (Read, Show)

data Params = Params { pUsername     :: String
                     , pPassword     :: String
                     , pPassword2    :: String
                     , pRevision     :: Maybe String
                     , pDestination  :: String
                     , pReferer      :: Maybe String
                     , pUri          :: String
                     , pForUser      :: Maybe String
                     , pSince        :: Maybe DateTime
                     , pRaw          :: String
                     , pLimit        :: Int
                     , pPatterns     :: [String]
                     , pGotoPage     :: String
                     , pEditedText   :: Maybe String
                     , pMessages     :: [String]
                     , pFrom         :: Maybe String
                     , pTo           :: Maybe String
                     , pFormat       :: String
                     , pSHA1         :: String
                     , pLogMsg       :: String
                     , pEmail        :: String
                     , pFullName     :: String
                     , pAccessCode   :: String
                     , pWikiname     :: String
                     , pPrintable    :: Bool
                     , pOverwrite    :: Bool
                     , pFilename     :: String
                     , pFileContents :: B.ByteString
                     , pUser         :: String
                     , pConfirm      :: Bool 
                     , pSessionKey   :: Maybe SessionKey
                     , pRecaptcha    :: Recaptcha
                     , pPeer         :: String
                     }  deriving Show

instance FromData Params where
     fromData = do
         un <- look "username"       `mplus` return ""
         pw <- look "password"       `mplus` return ""
         p2 <- look "password2"      `mplus` return ""
         rv <- (look "revision" >>= \s ->
                 return (if null s then Nothing else Just s)) `mplus` return Nothing
         fu <- (look "forUser" >>= return . Just) `mplus` return Nothing
         si <- (look "since" >>= return . parseDateTime "%Y-%m-%d") `mplus` return Nothing  -- YYYY-mm-dd format
         ds <- (lookCookieValue "destination") `mplus` return "/"
         ra <- look "raw"            `mplus` return ""
         lt <- look "limit"          `mplus` return "100"
         pa <- look "patterns"       `mplus` return ""
         gt <- look "gotopage"       `mplus` return ""
         me <- lookRead "messages"   `mplus` return [] 
         fm <- (look "from" >>= return . Just) `mplus` return Nothing
         to <- (look "to" >>= return . Just)   `mplus` return Nothing
         et <- (look "editedText" >>= return . Just . filter (/= '\r')) `mplus` return Nothing
         fo <- look "format"         `mplus` return ""
         sh <- look "sha1"           `mplus` return ""
         lm <- look "logMsg"         `mplus` return ""
         em <- look "email"          `mplus` return ""
         na <- look "full_name_1"    `mplus` return ""
         wn <- look "wikiname"       `mplus` return ""
         pr <- (look "printable" >> return True) `mplus` return False
         ow <- (look "overwrite" >>= return . (== "yes")) `mplus` return False
         fn <- (lookInput "file" >>= return . fromMaybe "" . inputFilename) `mplus` return ""
         fc <- (lookInput "file" >>= return . inputValue) `mplus` return B.empty
         ac <- look "accessCode"     `mplus` return ""
         cn <- (look "confirm" >> return True) `mplus` return False
         sk <- (readCookieValue "sid" >>= return . Just) `mplus` return Nothing
         rc <- look "recaptcha_challenge_field" `mplus` return ""
         rr <- look "recaptcha_response_field" `mplus` return ""
         return $ Params { pUsername     = un
                         , pPassword     = pw
                         , pPassword2    = p2
                         , pRevision     = rv
                         , pForUser      = fu
                         , pSince        = si
                         , pDestination  = ds
                         , pReferer      = Nothing  -- this gets set by handle...
                         , pUri          = ""       -- this gets set by handle...
                         , pRaw          = ra
                         , pLimit        = read lt
                         , pPatterns     = words pa
                         , pGotoPage     = gt
                         , pMessages     = me
                         , pFrom         = fm
                         , pTo           = to
                         , pEditedText   = et
                         , pFormat       = fo 
                         , pSHA1         = sh
                         , pLogMsg       = lm
                         , pEmail        = em
                         , pFullName     = na 
                         , pWikiname     = wn
                         , pPrintable    = pr 
                         , pOverwrite    = ow
                         , pFilename     = fn
                         , pFileContents = fc
                         , pAccessCode   = ac
                         , pUser         = ""  -- this gets set by ifLoggedIn...
                         , pConfirm      = cn
                         , pSessionKey   = sk
                         , pRecaptcha    = Recaptcha { recaptchaChallengeField = rc, recaptchaResponseField = rr }
                         , pPeer         = ""  -- this gets set by handle...
                         }

data Command = Command (Maybe String)

instance FromData Command where
     fromData = do
       pairs <- lookPairs
       return $ case map fst pairs `intersect` commandList of
                 []          -> Command Nothing
                 (c:_)       -> Command $ Just c
               where commandList = ["page", "request", "params", "edit", "showraw", "history",
                                    "export", "diff", "cancel", "update", "delete", "discuss"]

getLoggedInUser :: MonadIO m => Params -> m (Maybe String)
getLoggedInUser params = do
  mbSd <- maybe (return Nothing) getSession $ pSessionKey params
  let user = case mbSd of
       Nothing    -> Nothing
       Just sd    -> Just $ sessionUser sd
  return $! user

sessionTime :: Int
sessionTime = 60 * 60     -- session will expire 1 hour after page request

unlessNoEdit :: (String -> Params -> Web Response)
             -> (String -> Params -> Web Response)
             -> (String -> Params -> Web Response)
unlessNoEdit responder fallback =
  \page params -> do cfg <- getConfig
                     if page `elem` noEdit cfg
                        then fallback page params{pMessages = ("Page is locked." : pMessages params)}
                        else responder page params

unlessNoDelete :: (String -> Params -> Web Response)
               -> (String -> Params -> Web Response)
               -> (String -> Params -> Web Response)
unlessNoDelete responder fallback =
  \page params ->  do cfg <- getConfig
                      if page `elem` noDelete cfg
                         then fallback page params{pMessages = ("Page cannot be deleted." : pMessages params)}
                         else responder page params

handle :: (String -> Bool) -> Method -> (String -> Params -> Web Response) -> Handler
handle pathtest meth responder = uriRest $ \uri ->
  let path' = decodeString $ uriPath uri
  in  if pathtest path'
         then do
           compressedResponseFilter
           withData $ \params ->
               withRequest $ \req ->
                 if rqMethod req == meth
                    then do
                      let referer = case M.lookup (fromString "referer") (rqHeaders req) of
                                         Just r | not (null (hValue r)) -> Just $ toString $ head $ hValue r
                                         _       -> Nothing
                      let peer = fst $ rqPeer req
                      responder path' (params { pReferer = referer,
                                                pUri = uri,
                                                pPeer = peer })
                    else mzero
         else anyRequest mzero

handlePage :: Method -> (String -> Params -> Web Response) -> Handler
handlePage = handle isPage

handleText :: Method -> (String -> Params -> Web Response) -> Handler
handleText = handle (\x -> isPage x || isSourceCode x)

handlePath :: String -> Method -> (String -> Params -> Web Response) -> Handler
handlePath path' = handle (== path')

withCommand :: String -> [Handler] -> Handler
withCommand command handlers =
  withData $ \com -> case com of
                          Command (Just c) | c == command -> msum handlers
                          _                               -> anyRequest mzero 

-- | Returns path portion of URI, without initial /.
-- Consecutive spaces are collapsed.  We don't want to distinguish 'Hi There' and 'Hi  There'.
uriPath :: String -> String
uriPath = unwords . words . drop 1 . takeWhile (/='?')

isPage :: String -> Bool
isPage ('_':_) = False
isPage s = '.' `notElem` s

isDiscussPage :: String -> Bool
isDiscussPage s = isPage s && ":discuss" `isSuffixOf` s

isSourceCode :: String -> Bool
isSourceCode = not . null . languagesByExtension . takeExtension

urlForPage :: String -> String
urlForPage page = '/' : (substitute "%2f" "/" $ substitute "%3a" ":" $ urlEncode $ encodeString page)
-- this is needed so that browsers recognize relative URLs correctly

pathForPage :: String -> FilePath
pathForPage page = page <.> "page"

withCommands :: Method -> [String] -> (String -> Request -> Web Response) -> Handler
withCommands meth commands page = withRequest $ \req -> do
  if rqMethod req /= meth
     then mzero
     else if all (`elem` (map fst $ rqInputs req)) commands
             then page (intercalate "/" $ rqPaths req) req
             else mzero

getMimeTypeForExtension :: MonadIO m => String -> m String
getMimeTypeForExtension ext = do
  mimes <- queryAppState mimeMap
  return $ case M.lookup (dropWhile (=='.') $ map toLower ext) mimes of
                Nothing -> "application/octet-stream"
                Just t  -> t

ifLoggedIn :: (String -> Params -> Web Response)
           -> (String -> Params -> Web Response)
           -> (String -> Params -> Web Response)
ifLoggedIn responder fallback =
  \page params -> do user <- getLoggedInUser params
                     case user of
                          Nothing  -> do
                             fallback page (params { pReferer = Just $ pUri params })
                          Just u   -> do
                             usrs <- queryAppState users
                             let e = case M.lookup u usrs of
                                           Just usr    -> uEmail usr
                                           Nothing     -> error $ "User '" ++ u ++ "' not found."
                             -- give the user another hour...
                             addCookie sessionTime (mkCookie "sid" (show $ fromJust $ pSessionKey params))
                             responder page (params { pUser = u, pEmail = e })

validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

