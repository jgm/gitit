{-
expireGititCache - (C) 2009 John MacFarlane, licensed under the GPL

This program is designed to be used in post-update hooks and other scripts.

Usage:  expireGititCache base-url [file..]

Example:

    expireGititCache http://localhost:5001 page1.page foo/bar.hs "Front Page.page"

will produce POST requests to http://localhost:5001/_expire/page1,
http://localhost:5001/_expire/foo/bar.hs, and
http://localhost:5001/_expire/Front Page.

Return statuses:

0   -> the cached page was successfully expired (or was not cached in the first place)
1   -> fewer than two arguments were supplied
3   -> did not receive a 200 OK response from the request
5   -> could not parse the uri

-}

module Main
where
import Network.HTTP
import System.Environment
import Network.URI
import System.FilePath
import Control.Monad
import System.IO
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  (uriString : files) <- if length args < 2
                            then usageMessage >> return [""]
                            else return args
  uri <- case parseURI uriString of
             Just u  -> return u
             Nothing -> do
               hPutStrLn stderr ("Could not parse URI " ++ uriString)
               exitWith (ExitFailure 5)
  forM_ files (expireFile uri)  

usageMessage :: IO ()
usageMessage = do
  hPutStrLn stderr $ "Usage: expireGititCache base-url [file..]\n" ++
    "Example: expireGititCache http://localhost:5001 page1.page foo/bar.hs"
  exitWith (ExitFailure 1)

expireFile :: URI -> FilePath -> IO ()
expireFile uri file = do
  let path' = if takeExtension file == ".page"
                 then dropExtension file
                 else file
  let uri' = uri{uriPath = "/_expire/" ++ urlEncode path'}
  resResp <- simpleHTTP Request{rqURI = uri', rqMethod = POST, rqHeaders = [], rqBody = ""}
  case resResp of
       Left connErr    -> error $ show connErr
       Right (Response (2,0,0) _ _ _) -> return ()
       _ -> do
         hPutStrLn stderr ("Request for " ++ show uri' ++ " did not return success status")
         exitWith (ExitFailure 3)
