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

{- | Useful functions for defining wiki handlers.
-}

module Network.Gitit.Framework (
                               -- * Combinators for dealing with users 
                                 withUserFromSession
                               , withUserFromHTTPAuth
                               , requireUserThat
                               , requireUser
                               , getLoggedInUser
                               , sessionTime
                               -- * Combinators to exclude certain actions
                               , unlessNoEdit
                               , unlessNoDelete
                               -- * Guards for routing
                               , guardCommand
                               , guardPath
                               , guardIndex
                               , guardBareBase
                               -- * Functions to get info from the request
                               , getPath
                               , getPage
                               , getReferer
                               , getWikiBase
                               , uriPath
                               -- * Useful predicates
                               , isPage
                               , isPageFile
                               , isDiscussPage
                               , isDiscussPageFile
                               , isSourceCode
                               -- * Combinators that change the request locally
                               , withMessages
                               , withInput
                               -- * Miscellaneous
                               , urlForPage
                               , pathForPage
                               , getMimeTypeForExtension
                               , validate
                               , filestoreFromConfig
                               )
where
import Safe
import Network.Gitit.Server
import Network.Gitit.State
import Network.Gitit.Types
import Data.FileStore
import Data.Char (toLower, isAscii)
import Control.Monad (mzero, liftM, MonadPlus)
import qualified Data.Map as M
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Maybe (fromJust)
import Data.List (intercalate, isPrefixOf, isInfixOf, (\\))
import System.FilePath ((<.>), takeExtension)
import Text.Highlighting.Kate
import Text.ParserCombinators.Parsec
import Network.URL (decString, encString)
import Happstack.Crypto.Base64 (decode)
import Network.HTTP (urlEncodeVars)

-- | Run the handler if a user is logged in, otherwise redirect
-- to login page.
requireUser :: Handler -> Handler 
requireUser = requireUserThat (const True)

-- | Run the handler if a user satisfying the predicate is logged in.
-- Redirect to login if nobody logged in; raise error if someone is
-- logged in but doesn't satisfy the predicate.
requireUserThat :: (User -> Bool) -> Handler -> Handler
requireUserThat predicate handler = do
  mbUser <- getLoggedInUser
  rq <- askRq
  let url = rqUri rq ++ rqQuery rq
  case mbUser of
       Nothing   -> tempRedirect ("/_login?" ++ urlEncodeVars [("destination", url)]) $ toResponse ()
       Just u    -> if predicate u
                       then handler
                       else error "Not authorized."

-- | Run the handler after setting @REMOTE_USER@ with the user from
-- the session.
withUserFromSession :: Handler -> Handler
withUserFromSession handler = withData $ \(sk :: Maybe SessionKey) -> do
  mbSd <- maybe (return Nothing) getSession sk
  mbUser <- case mbSd of
            Nothing    -> return Nothing
            Just sd    -> do
              addCookie sessionTime (mkCookie "sid" (show $ fromJust sk))  -- refresh timeout
              getUser $! sessionUser sd
  let user = maybe "" uUsername mbUser
  localRq (setHeader "REMOTE_USER" user) handler

-- | Run the handler after setting @REMOTE_USER@ from the "authorization"
-- header.  Works with simple HTTP authentication or digest authentication.
withUserFromHTTPAuth :: Handler -> Handler
withUserFromHTTPAuth handler = do
  req <- askRq
  let user = case (getHeader "authorization" req) of
              Nothing         -> ""
              Just authHeader -> case parse pAuthorizationHeader "" (toString authHeader) of
                                  Left _  -> ""
                                  Right u -> u
  localRq (setHeader "REMOTE_USER" user) handler

-- | Returns @Just@ logged in user or @Nothing@.
getLoggedInUser :: GititServerPart (Maybe User)
getLoggedInUser = do
  req <- askRq
  case maybe "" toString (getHeader "REMOTE_USER" req) of
        "" -> return Nothing
        u  -> do
          mbUser <- getUser u
          case mbUser of
               Just user -> return $ Just user
               Nothing   -> return $ Just User{uUsername = u, uEmail = "", uPassword = undefined}

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

-- | @unlessNoEdit responder fallback@ runs @responder@ unless the
-- page has been designated not editable in configuration; in that
-- case, runs @fallback@.
unlessNoEdit :: Handler
             -> Handler
             -> Handler
unlessNoEdit responder fallback = withData $ \(params :: Params) -> do
  cfg <- getConfig
  page <- getPage
  if page `elem` noEdit cfg
     then withMessages ("Page is locked." : pMessages params) fallback
     else responder

-- | @unlessNoDelete responder fallback@ runs @responder@ unless the
-- page has been designated not deletable in configuration; in that
-- case, runs @fallback@.
unlessNoDelete :: Handler
               -> Handler
               -> Handler
unlessNoDelete responder fallback = withData $ \(params :: Params) -> do
  cfg <- getConfig
  page <- getPage
  if page `elem` noDelete cfg
     then withMessages ("Page cannot be deleted." : pMessages params) fallback
     else responder

-- | Returns the current path (subtracting initial commands like @\/_edit@).
getPath :: ServerMonad m => m String
getPath = liftM (fromJust . decString True . intercalate "/" . rqPaths) askRq

-- | Returns the current page name (derived from the path).
getPage :: GititServerPart String
getPage = do
  conf <- getConfig
  path' <- getPath
  if null path'
     then return (frontPage conf)
     else if isPage path'
             then return path'
             else mzero  -- fail if not valid page name

-- | Returns the contents of the "referer" header.
getReferer :: ServerMonad m => m String
getReferer = do
  req <- askRq
  base' <- getWikiBase
  return $ case getHeader "referer" req of
                 Just r  -> case toString r of
                                 ""  -> base'
                                 s   -> s
                 Nothing -> base'

-- | Returns the base URL of the wiki in the happstack server.
-- So, if the wiki handlers are behind a @dir 'foo'@, getWikiBase will
-- return @\/foo/@.  getWikiBase doesn't know anything about HTTP
-- proxies, so if you use proxies to map a gitit wiki to @\/foo/@,
-- you'll still need to follow the instructions in README.
getWikiBase :: ServerMonad m => m String
getWikiBase = do
  path' <- getPath
  uri' <- liftM (fromJust . decString True . rqUri) askRq
  case calculateWikiBase path' uri' of
       Just b    -> return b
       Nothing   -> error $ "Could not getWikiBase: (path, uri) = " ++ show (path',uri')

-- | The pure core of 'getWikiBase'.
calculateWikiBase :: String -> String -> Maybe String
calculateWikiBase path' uri' =
  let revpaths = reverse . filter (not . null) $ splitOn '/' path'
      revuris  = reverse . filter (not . null) $ splitOn '/' uri'
  in  if revpaths `isPrefixOf` revuris
         then let revbase = drop (length revpaths) revuris
                  -- a path like _feed is not part of the base...
                  revbase' = case revbase of
                             (x:xs) | startsWithUnderscore x -> xs
                             xs                              -> xs
                  base'    = intercalate "/" $ reverse revbase'
              in  Just $ if null base' then "" else '/' : base'
          else Nothing

startsWithUnderscore :: String -> Bool
startsWithUnderscore ('_':_) = True
startsWithUnderscore _ = False

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c cs =
  let (next, rest) = break (==c) cs
  in case rest of
         []     -> [next]
         (_:rs) -> next : splitOn c rs 

-- | Returns path portion of URI, without initial @\/@.
-- Consecutive spaces are collapsed.  We don't want to distinguish
-- @Hi There@ and @Hi  There@.
uriPath :: String -> String
uriPath = unwords . words . drop 1 . takeWhile (/='?')

isPage :: String -> Bool
isPage "" = False
isPage ('_':_) = False
isPage s = all (`notElem` "*?") s && not (".." `isInfixOf` s) && not ("/_" `isInfixOf` s)
-- for now, we disallow @*@ and @?@ in page names, because git filestore
-- does not deal with them properly, and darcs filestore disallows them.

isPageFile :: FilePath -> Bool
isPageFile f = takeExtension f == ".page"

isDiscussPage :: String -> Bool
isDiscussPage ('@':xs) = isPage xs
isDiscussPage _ = False

isDiscussPageFile :: FilePath -> Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = False

isSourceCode :: String -> Bool
isSourceCode path' =
  let ext   = map toLower $ takeExtension path'
      langs = languagesByExtension ext \\ ["Postscript"]
  in  not . null $ langs

-- | Returns encoded URL path for the page with the given name, relative to
-- the wiki base.
urlForPage :: String -> String
urlForPage page = "/" ++
  encString True (\c -> isAscii c && (c `notElem` "?&")) page
-- / and @ are left unescaped so that browsers recognize relative URLs and talk pages correctly

-- | Returns the filestore path of the file containing the page's source.
pathForPage :: String -> FilePath
pathForPage page = page <.> "page"

-- | Retrieves a mime type based on file extension.
getMimeTypeForExtension :: String -> GititServerPart String
getMimeTypeForExtension ext = do
  mimes <- liftM mimeMap getConfig
  return $ case M.lookup (dropWhile (=='.') $ map toLower ext) mimes of
                Nothing -> "application/octet-stream"
                Just t  -> t

-- | Simple helper for validation of forms.
validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

guardCommand :: String -> GititServerPart ()
guardCommand command = withData $ \(com :: Command) ->
  case com of
       Command (Just c) | c == command -> return ()
       _                               -> mzero

guardPath :: (String -> Bool) -> GititServerPart ()
guardPath pred' = guardRq (pred' . rqUri)

-- | Succeeds if path is an index path:  e.g. @\/foo\/bar/@.
guardIndex :: GititServerPart ()
guardIndex = do
  base <- getWikiBase
  uri' <- liftM rqUri askRq
  let localpath = drop (length base) uri'
  if length localpath > 1 && lastNote "guardIndex" uri' == '/'
     then return ()
     else mzero

-- Guard against a path like @\/wiki@ when the wiki is being
-- served at @\/wiki@.
guardBareBase :: GititServerPart ()
guardBareBase = do
  base' <- getWikiBase
  uri' <- liftM rqUri askRq
  if not (null base') && base' == uri'
     then return ()
     else mzero

-- | Runs a server monad in a local context after setting
-- the "messages" request header.
withMessages :: ServerMonad m => [String] -> m a -> m a
withMessages = withInput "messages" . show

-- | Runs a server monad in a local context after setting
-- request header.
withInput :: ServerMonad m => String -> String -> m a -> m a
withInput name val handler = do
  req <- askRq
  let inps = filter (\(n,_) -> n /= name) $ rqInputs req
  let newInp = (name, Input { inputValue = fromString val
                            , inputFilename = Nothing
                            , inputContentType = ContentType {
                                    ctType = "text"
                                  , ctSubtype = "plain"
                                  , ctParameters = [] }
                            })
  localRq (\rq -> rq{ rqInputs = newInp : inps }) handler

-- | Returns a filestore object derived from the
-- repository path and filestore type specified in configuration.
filestoreFromConfig :: Config -> FileStore
filestoreFromConfig conf =
  case repositoryType conf of
         Git   -> gitFileStore $ repositoryPath conf
         Darcs -> darcsFileStore $ repositoryPath conf
