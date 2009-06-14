{-# LANGUAGE ScopedTypeVariables #-}
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
                       , getPath
                       , getPage
                       , getReferer
                       , uriPath
                       , isPage
                       , isPageFile
                       , isDiscussPage
                       , isDiscussPageFile
                       , isSourceCode
                       , isIndex
                       , isPreview
                       , urlForPage
                       , pathForPage
                       , getMimeTypeForExtension
                       , ifLoggedIn
                       , validate
                       , guardCommand
                       , guardPath
                       )
where
import Gitit.Server
import Gitit.State
import Gitit.Types
import Data.Char (toLower, isAscii, isDigit, isLetter)
import Control.Monad.Trans (MonadIO)
import Control.Monad (mzero, liftM, MonadPlus)
import qualified Data.Map as M
import Data.ByteString.UTF8 (toString)
import Data.Maybe (fromJust)
import Data.List (intercalate, isSuffixOf, (\\))
import System.FilePath ((<.>), takeExtension, dropExtension)
import Text.Highlighting.Kate
import Text.ParserCombinators.Parsec
import Network.URL (decString, encString)
import Happstack.Crypto.Base64 (decode)

getLoggedInUser :: (MonadPlus m, ServerMonad m, MonadIO m) => m (Maybe User)
getLoggedInUser = withData $ \(sk :: Maybe SessionKey) -> do
  cfg <- getConfig
  req <- askRq
  case authenticationMethod cfg of
       HTTPAuth -> do
         case (getHeader "authorization" req) of
              Just authHeader -> case parse pAuthorizationHeader "" (toString authHeader) of
                                 Left _  -> return Nothing
                                 Right u -> return $ Just $
                                              User{ uUsername = u,
                                                    uPassword = undefined,
                                                    uEmail    = "" }
              Nothing         -> return Nothing
       FormAuth -> do
         mbSd <- maybe (return Nothing) getSession sk
         case mbSd of
              Nothing    -> return Nothing
              Just sd    -> getUser $! sessionUser sd

pAuthorizationHeader :: GenParser Char st String
pAuthorizationHeader = try pBasicHeader <|> pDigestHeader

pDigestHeader :: GenParser Char st String
pDigestHeader = do
  string "Digest username=\""
  result' <- many (noneOf "\"")
  char '"'
  return result'

pBasicHeader :: GenParser Char st String
pBasicHeader = do
  string "Basic "
  result' <- many (noneOf " \t\n")
  return $ takeWhile (/=':') $ decode result'

sessionTime :: Int
sessionTime = 60 * 60     -- session will expire 1 hour after page request

unlessNoEdit :: (Params -> Handler)
             -> (Params -> Handler)
             -> (Params -> Handler)
unlessNoEdit responder fallback =
  \params -> do cfg <- getConfig
                page <- getPage
                if page `elem` noEdit cfg
                   then fallback params{pMessages = ("Page is locked." : pMessages params)}
                   else responder params

unlessNoDelete :: (Params -> Handler)
               -> (Params -> Handler)
               -> (Params -> Handler)
unlessNoDelete responder fallback =
  \params ->  do cfg <- getConfig
                 page <- getPage
                 if page `elem` noDelete cfg
                    then fallback params{pMessages = ("Page cannot be deleted." : pMessages params)}
                    else responder params

getPath :: ServerMonad m => m String
getPath = liftM (fromJust . decString True . intercalate "/" . rqPaths) askRq

getPage :: (ServerMonad m, MonadIO m) => m String
getPage = do
  conf <- getConfig
  path' <- getPath
  if null path'
     then return (frontPage conf)
     else return path'

getReferer :: ServerMonad m => m String
getReferer = do
  req <- askRq
  return $ case getHeader "referer" req of
                 Just r  -> case toString r of
                                 ""  -> "/"
                                 s   -> s
                 Nothing -> "/"

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
isSourceCode path' =
  let ext   = map toLower $ takeExtension path'
      langs = languagesByExtension ext \\ ["Postscript"]
  in  not . null $ langs

isIndex :: String -> Bool
isIndex ""       = False
isIndex x        = last x == '/'

isPreview :: String -> Bool
isPreview "___preview" = True
isPreview _ = False
-- We choose something that is unlikely to occur naturally as a suffix.
-- Why suffix and not prefix?  Because the link is added by a script,
-- and mod_proxy_html doesn't rewrite links in scripts.  So this is
-- to make it possible to use gitit with an alternative docroot.

urlForPage :: String -> String
urlForPage page = '/' : encString True (\c -> isAscii c && (isLetter c || isDigit c || c `elem` "/:")) page
-- / and : are left unescaped so that browsers recognize relative URLs and talk pages correctly

pathForPage :: String -> FilePath
pathForPage page = page <.> "page"

getMimeTypeForExtension :: MonadIO m => String -> m String
getMimeTypeForExtension ext = do
  mimes <- queryAppState mimeMap
  return $ case M.lookup (dropWhile (=='.') $ map toLower ext) mimes of
                Nothing -> "application/octet-stream"
                Just t  -> t

ifLoggedIn :: (Params -> Handler)
           -> (Params -> Handler)
           -> (Params -> Handler)
ifLoggedIn responder fallback params = withData $ \(sk :: Maybe SessionKey) -> do
  user <- getLoggedInUser
  case user of
       Nothing  -> do
          localRq (\rq -> setHeader "referer" (rqUri rq ++ rqQuery rq) rq) (fallback params)
       Just _   -> do
          -- give the user another hour...
          case sk of
               Just key  -> addCookie sessionTime (mkCookie "sid" (show key))
               Nothing   -> return ()
          responder params

validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

guardCommand :: String -> ServerPart ()
guardCommand command = withData $ \(com :: Command) ->
  case com of
       Command (Just c) | c == command -> return ()
       _                               -> mzero

guardPath :: (String -> Bool) -> ServerPart ()
guardPath pred' = guardRq $ \rq -> not (null $ rqPaths rq) && pred' (last (rqPaths rq))
