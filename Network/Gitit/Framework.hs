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
                               , authenticateUserThat
                               , authenticate
                               , getLoggedInUser
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
import Data.Char (toLower)
import Control.Monad (mzero, liftM, unless)
import qualified Data.Map as M
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Maybe (fromJust, fromMaybe)
import Data.List (intercalate, isPrefixOf, isInfixOf)
import System.FilePath ((<.>), takeExtension, takeFileName)
import Text.Highlighting.Kate
import Text.ParserCombinators.Parsec
import Network.URL (decString, encString)
import Network.URI (isUnescapedInURI)
import Data.ByteString.Base64 (decodeLenient)
import Network.HTTP (urlEncodeVars)

-- | Require a logged in user if the authentication level demands it.
-- Run the handler if a user is logged in, otherwise redirect
-- to login page.
authenticate :: AuthenticationLevel -> Handler -> Handler
authenticate = authenticateUserThat (const True)

-- | Like 'authenticate', but with a predicate that the user must satisfy.
authenticateUserThat :: (User -> Bool) -> AuthenticationLevel -> Handler -> Handler
authenticateUserThat predicate level handler = do
  cfg <- getConfig
  if level <= requireAuthentication cfg
     then do
       mbUser <- getLoggedInUser
       rq <- askRq
       let url = rqUri rq ++ rqQuery rq
       case mbUser of
            Nothing   -> tempRedirect ("/_login?" ++ urlEncodeVars [("destination", url)]) $ toResponse ()
            Just u    -> if predicate u
                            then handler
                            else error "Not authorized."
     else handler

-- | Run the handler after setting @REMOTE_USER@ with the user from
-- the session.
withUserFromSession :: Handler -> Handler
withUserFromSession handler = withData $ \(sk :: Maybe SessionKey) -> do
  mbSd <- maybe (return Nothing) getSession sk
  cfg <- getConfig
  mbUser <- case mbSd of
            Nothing    -> return Nothing
            Just sd    -> do
              addCookie (MaxAge $ sessionTimeout cfg) (mkCookie "sid" (show $ fromJust sk))  -- refresh timeout
              getUser $! sessionUser sd
  let user = maybe "" uUsername mbUser
  localRq (setHeader "REMOTE_USER" user) handler

-- | Run the handler after setting @REMOTE_USER@ from the "authorization"
-- header.  Works with simple HTTP authentication or digest authentication.
withUserFromHTTPAuth :: Handler -> Handler
withUserFromHTTPAuth handler = do
  req <- askRq
  let user = case getHeader "authorization" req of
              Nothing         -> ""
              Just authHeader -> case parse pAuthorizationHeader "" (UTF8.toString authHeader) of
                                  Left _  -> ""
                                  Right u -> u
  localRq (setHeader "REMOTE_USER" user) handler

-- | Returns @Just@ logged in user or @Nothing@.
getLoggedInUser :: GititServerPart (Maybe User)
getLoggedInUser = do
  req <- askRq
  case maybe "" UTF8.toString (getHeader "REMOTE_USER" req) of
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
  _ <- string "Digest username=\""
  result' <- many (noneOf "\"")
  _ <- char '"'
  return result'

pBasicHeader :: GenParser Char st String
pBasicHeader = do
  _ <- string "Basic "
  result' <- many (noneOf " \t\n")
  return $ takeWhile (/=':') $ UTF8.toString
         $ decodeLenient $ UTF8.fromString result'

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
getPath = liftM (intercalate "/" . rqPaths) askRq

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
                 Just r  -> case UTF8.toString r of
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
  let langs = languagesByFilename $ takeFileName path'
      ext = takeExtension path'
  in  not (null langs || ext == ".svg" || ext == ".eps")
                         -- allow svg or eps to be served as image

-- | Returns encoded URL path for the page with the given name, relative to
-- the wiki base.
urlForPage :: String -> String
urlForPage page = '/' : encString False isUnescapedInURI page

-- | Returns the filestore path of the file containing the page's source.
pathForPage :: String -> FilePath
pathForPage page = page <.> "page"

-- | Retrieves a mime type based on file extension.
getMimeTypeForExtension :: String -> GititServerPart String
getMimeTypeForExtension ext = do
  mimes <- liftM mimeMap getConfig
  return $ fromMaybe "application/octet-stream"
    (M.lookup (dropWhile (== '.') $ map toLower ext) mimes)

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
  unless (length localpath > 1 && lastNote "guardIndex" uri' == '/')
    mzero

-- Guard against a path like @\/wiki@ when the wiki is being
-- served at @\/wiki@.
guardBareBase :: GititServerPart ()
guardBareBase = do
  base' <- getWikiBase
  uri' <- liftM rqUri askRq
  unless (not (null base') && base' == uri')
    mzero

-- | Runs a server monad in a local context after setting
-- the "message" request header.
withMessages :: ServerMonad m => [String] -> m a -> m a
withMessages messages handler = do
  req <- askRq
  let inps = filter (\(n,_) -> n /= "message") $ rqInputsQuery req
  let newInp msg = ("message", Input {
                              inputValue = Right
                                         $ LazyUTF8.fromString msg
                            , inputFilename = Nothing
                            , inputContentType = ContentType {
                                    ctType = "text"
                                  , ctSubtype = "plain"
                                  , ctParameters = [] }
                            })
  localRq (\rq -> rq{ rqInputsQuery = map newInp messages ++ inps }) handler

-- | Returns a filestore object derived from the
-- repository path and filestore type specified in configuration.
filestoreFromConfig :: Config -> FileStore
filestoreFromConfig conf =
  case repositoryType conf of
         Git       -> gitFileStore       $ repositoryPath conf
         Darcs     -> darcsFileStore     $ repositoryPath conf
         Mercurial -> mercurialFileStore $ repositoryPath conf
