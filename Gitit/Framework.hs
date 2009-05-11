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

module Gitit.Framework ( getLoggedInUser
                       , sessionTime
                       , unlessNoEdit
                       , unlessNoDelete
                       , handle
                       , handlePage
                       , handlePath
                       , withCommand
                       , uriPath
                       , isPage
                       , isPageFile
                       , isDiscussPage
                       , isDiscussPageFile
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
import Gitit.Types
import Data.Char (toLower, isAscii, isDigit, isLetter)
import Control.Monad.Trans (MonadIO)
import Control.Monad (msum, mzero)
import qualified Data.Map as M
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (fromJust)
import Data.List (intercalate, isSuffixOf, (\\))
import System.FilePath ((<.>), takeExtension, dropExtension)
import Text.Highlighting.Kate
import Text.ParserCombinators.Parsec
import Network.URL (decString, encString)

getLoggedInUser :: MonadIO m => Params -> m (Maybe String)
getLoggedInUser params = do
  cfg <- getConfig
  case authenticationMethod cfg of
       HTTPAuth -> do
         case pAuthHeader params of
              Just authHeader -> case parse pAuthorizationHeader "" authHeader of
                                 Left _  -> return Nothing
                                 Right u -> return (Just u)
              Nothing         -> return Nothing
       FormAuth -> do
         mbSd <- maybe (return Nothing) getSession $ pSessionKey params
         let user = case mbSd of
              Nothing    -> Nothing
              Just sd    -> Just $ sessionUser sd
         return $! user

pAuthorizationHeader :: GenParser Char st String
pAuthorizationHeader = do
  string "Digest username=\""
  result <- many (noneOf "\"")
  char '"'
  return result

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
handle pathtest meth responder = do
  req <- askRq
  let uri = rqUri req ++ rqQuery req
  let path' = fromJust $ decString True $ uriPath uri
  if pathtest path'
     then do
       cfg <- getConfig
       let path'' = if null path' then frontPage cfg else path'
       if compressResponses cfg then compressedResponseFilter else return "" 
       withData $ \params -> anyRequest $
         if rqMethod req == meth
            then do
              let referer = case M.lookup (fromString "referer") (rqHeaders req) of
                                 Just r | not (null (hValue r)) -> Just $ toString $ head $ hValue r
                                 _       -> Nothing
              let peer = fst $ rqPeer req
              let authHeader = case M.lookup (fromString "authorization") (rqHeaders req) of
                                 Just r  -> Just $ toString $ head $ hValue r
                                 Nothing -> Nothing
              responder path'' (params { pReferer = referer,
                                         pUri = uri,
                                         pPeer = peer,
                                         pAuthHeader = authHeader })
            else mzero
     else anyRequest mzero

handlePage :: Method -> (String -> Params -> Web Response) -> Handler
handlePage = handle isPage

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
isPage _ = True

isPageFile :: FilePath -> Bool
isPageFile f = takeExtension f == ".page"

isDiscussPage :: String -> Bool
isDiscussPage s = isPage s && ":discuss" `isSuffixOf` s

isDiscussPageFile :: FilePath -> Bool
isDiscussPageFile f = isPageFile f && ":discuss" `isSuffixOf` (dropExtension f)

isSourceCode :: String -> Bool
isSourceCode path =
  let ext   = map toLower $ takeExtension path
      langs = languagesByExtension ext \\ ["Postscript"]
  in  not . null $ langs

urlForPage :: String -> String
urlForPage page = '/' : encString True (\c -> isAscii c && (isLetter c || isDigit c || c `elem` "/:")) page
-- / and : are left unescaped so that browsers recognize relative URLs and talk pages correctly

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
                             case pSessionKey params of
                                  Just sk   -> addCookie sessionTime (mkCookie "sid" (show sk))
                                  Nothing   -> return ()
                             responder page (params { pUser = u, pEmail = e })

validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

