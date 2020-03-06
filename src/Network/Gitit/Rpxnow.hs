-- Modified from Michael Snoyman's BSD3 authenticate-0.0.1
-- and http-wget-0.0.1.
-- Facilitates authentication with "http://rpxnow.com/".

module Network.Gitit.Rpxnow
    ( Identifier (..)
    , authenticate
    ) where

import Text.JSON
import Data.Maybe (isJust, fromJust)
import System.Process
import System.Exit
import System.IO
import Network.HTTP (urlEncodeVars)

-- | Make a post request with parameters to the URL and return a response.
curl :: String             -- ^ URL
     -> [(String, String)] -- ^ Post parameters
     -> IO (Either String String) -- ^ Response body
curl url params = do
    (Nothing, Just hout, Just herr, phandle) <- createProcess $ (proc "curl"
        [url, "-d", urlEncodeVars params]
        ) { std_out = CreatePipe, std_err = CreatePipe }
    exitCode <- waitForProcess phandle
    case exitCode of
        ExitSuccess -> hGetContents hout >>= return . Right
        _           -> hGetContents herr >>= return . Left



-- | Information received from Rpxnow after a valid login.
data Identifier = Identifier
    { userIdentifier  :: String
    , userData        :: [(String, String)]
    }
    deriving Show

-- | Attempt to log a user in.
authenticate :: String -- ^ API key given by RPXNOW.
             -> String -- ^ Token passed by client.
             -> IO (Either String Identifier)
authenticate apiKey token = do
    body <- curl
                "https://rpxnow.com/api/v2/auth_info"
                [ ("apiKey", apiKey)
                , ("token", token)
                ]
    case body of
        Left s -> return $ Left $ "Unable to connect to rpxnow: " ++ s
        Right b ->
          case decode b >>= getObject of
            Error s -> return $ Left $ "Not a valid JSON response: " ++ s
            Ok o ->
              case valFromObj "stat" o of
                Error _ -> return $ Left "Missing 'stat' field"
                Ok "ok" -> return $ resultToEither $ parseProfile o
                Ok stat -> return $ Left $ "Login not accepted: " ++ stat

parseProfile :: JSObject JSValue -> Result Identifier
parseProfile v = do
    profile <- valFromObj "profile" v >>= getObject
    ident <- valFromObj "identifier" profile
    let pairs = fromJSObject profile
        pairs' = filter (\(k, _) -> k /= "identifier") pairs
        pairs'' = map fromJust . filter isJust . map takeString $ pairs'
    return $ Identifier ident pairs''

takeString :: (String, JSValue) -> Maybe (String, String)
takeString (k, JSString v) = Just (k, fromJSString v)
takeString _ = Nothing

getObject :: JSValue -> Result (JSObject JSValue)
getObject (JSObject o) = return o
getObject _ = fail "Not an object"
