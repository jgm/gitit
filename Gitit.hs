{-# LANGUAGE CPP #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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

module Main where

import HAppS.Server
import HAppS.State hiding (Method)
import System.Environment
import System.IO.UTF8
import System.IO (stderr)
import System.IO.Error (isAlreadyExistsError)
import Control.Exception (bracket)
import Prelude hiding (writeFile, readFile, putStrLn, putStr)
import System.Process
import System.Directory
import Control.Concurrent
import System.FilePath
import Gitit.Git
import Gitit.State
import Text.XHtml hiding ( (</>), dir, method, password )
import qualified Text.XHtml as X ( password, method )
import Data.List (intersect, intersperse, intercalate, sort, nub, sortBy, isSuffixOf)
import Data.Maybe (fromMaybe, fromJust, mapMaybe, isNothing)
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as M
import Data.Ord (comparing)
import qualified Data.Digest.SHA512 as SHA512 (hash)
import Paths_gitit
import Text.Pandoc
import Text.Pandoc.ODT (saveOpenDocumentAsODT)
import Text.Pandoc.Definition (processPandoc)
import Text.Pandoc.Shared (HTMLMathMethod(..), substitute)
import Data.ByteString.Internal (c2w)
import Data.Char (isAlphaNum, isAlpha)
import Codec.Binary.UTF8.String (decodeString)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Network.HTTP (urlEncodeVars, urlEncode)
import System.Console.GetOpt
import System.Exit
import Text.Highlighting.Kate

gititVersion :: String
gititVersion = "0.2.2.1"

main :: IO ()
main = do
  argv <- getArgs
  options <- parseArgs argv
  conf <- foldM handleFlag defaultConfig options
  gitPath <- findExecutable "git"
  when (isNothing gitPath) $ error "'git' program not found in system path."
  initializeWiki conf
  control <- startSystemState entryPoint
  update $ SetConfig conf
  hPutStrLn stderr $ "Starting server on port " ++ show (portNumber conf)
  let debugger = if (debugMode conf) then debugFilter else id
  tid <- forkIO $ simpleHTTP (Conf { validator = Nothing, port = portNumber conf }) $ debugger $
          [ dir "stylesheets" [ fileServe [] $ (staticDir conf) </> "stylesheets" ]
          , dir "images"      [ fileServe [] $ (staticDir conf) </> "images" ]
          , dir "javascripts" [ fileServe [] $ (staticDir conf) </> "javascripts" ]
          ] ++ wikiHandlers
  waitForTermination
  putStrLn "Shutting down..."
  killThread tid
  createCheckpoint control
  shutdownSystem control
  putStrLn "Shutdown complete"

data Opt
    = Help
    | ConfigFile FilePath
    | Version
    deriving (Eq)

flags :: [OptDescr Opt]
flags =
   [ Option ['h'] [] (NoArg Help)
        "Print this help message"
   , Option ['v'] [] (NoArg Version)
        "Print version information"
   , Option ['f'] [] (ReqArg ConfigFile "FILE")
        "Specify configuration file"
   ]

parseArgs :: [String] -> IO [Opt]
parseArgs argv = do
  progname <- getProgName
  case getOpt Permute flags argv of
    (opts,_,[])  -> return opts
    (_,_,errs)   -> hPutStrLn stderr (concat errs ++ usageInfo (usageHeader progname) flags) >>
                       exitWith (ExitFailure 1)

usageHeader :: String -> String
usageHeader progname = "Usage:  " ++ progname ++ " [opts...]"

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2008 John MacFarlane\n" ++
                   "This is free software; see the source for copying conditions.  There is no\n" ++
                   "warranty, not even for merchantability or fitness for a particular purpose."

handleFlag :: Config -> Opt -> IO Config
handleFlag _ opt = do
  progname <- getProgName
  case opt of
    Help         -> hPutStrLn stderr (usageInfo (usageHeader progname) flags) >> exitWith ExitSuccess
    Version      -> hPutStrLn stderr (progname ++ " version " ++ gititVersion ++ copyrightMessage) >> exitWith ExitSuccess
    ConfigFile f -> do readFile f >>= return . read

entryPoint :: Proxy AppState
entryPoint = Proxy

-- | Create repository and public directories, unless they already exist.
initializeWiki :: Config -> IO ()
initializeWiki conf = do
  let repodir = repositoryPath conf
  let frontpage = frontPage conf <.> "page"
  let staticdir = staticDir conf
  repoExists <- doesDirectoryExist repodir
  unless repoExists $ do
    postupdatepath <- getDataFileName $ "data" </> "post-update"
    postupdatecontents <- B.readFile postupdatepath
    welcomepath <- getDataFileName $ "data" </> "FrontPage.page"
    welcomecontents <- B.readFile welcomepath
    helppath <- getDataFileName $ "data" </> "Help.page"
    helpcontents <- B.readFile helppath
    createDirectory repodir
    oldDir <- getCurrentDirectory
    setCurrentDirectory repodir
    runCommand "git init" >>= waitForProcess
    -- add front page and help page
    B.writeFile frontpage welcomecontents
    B.writeFile "Help.page" helpcontents
    runCommand ("git add 'Help.page' '" ++ frontpage ++ "'; git commit -m 'Initial commit.'") >>= waitForProcess
    -- set post-update hook so working directory will be updated
    -- when changes are pushed to the repo
    let postupdate = ".git" </> "hooks" </> "post-update"
    B.writeFile postupdate postupdatecontents
    perms <- getPermissions postupdate
    setPermissions postupdate (perms {executable = True})
    hPutStrLn stderr $ "Created repository " ++ repodir
    setCurrentDirectory oldDir
  staticExists <- doesDirectoryExist staticdir
  unless staticExists $ do
    createDirectoryIfMissing True $ staticdir </> "stylesheets"
    let stylesheets = map ("stylesheets" </>) ["gitit.css", "hk-pyg.css", "folder.png", "page.png"]
    stylesheetpaths <- mapM getDataFileName stylesheets
    zipWithM copyFile stylesheetpaths (map (staticdir </>) stylesheets)
    createDirectoryIfMissing True $ staticdir </> "javascripts"
    let javascripts = ["jquery.min.js", "jquery-ui-personalized-1.6rc2.min.js",
                       "folding.js", "dragdiff.js", "preview.js", "search.js", "uploadForm.js"]
    javascriptpaths <- mapM getDataFileName $ map ("javascripts" </>) javascripts
    zipWithM copyFile javascriptpaths $ map ((staticdir </> "javascripts") </>) javascripts
    hPutStrLn stderr $ "Created " ++ staticdir ++ " directory"
  jsMathExists <- doesDirectoryExist (staticdir </> "javascripts" </> "jsMath")
  unless jsMathExists $ do
    hPutStrLn stderr $ replicate 80 '*' ++
                       "\nWarning:  jsMath not found.\n" ++
                       "If you want support for math, copy the jsMath directory into " ++ staticdir ++ "/javascripts/\n" ++
                       "jsMath can be obtained from http://www.math.union.edu/~dpvc/jsMath/\n" ++
                       replicate 80 '*'

type Handler = ServerPart Response


wikiHandlers :: [Handler]
wikiHandlers = [ handlePath "_index"     GET  indexPage
               , handlePath "_activity"  GET  showActivity
               , handlePath "_preview"   POST preview
               , handlePath "_search"    POST searchResults
               , handlePath "_search"    GET  searchResults
               , handlePath "_register"  GET  registerUserForm
               , handlePath "_register"  POST registerUser
               , handlePath "_login"     GET  loginUserForm
               , handlePath "_login"     POST loginUser
               , handlePath "_logout"    GET  logoutUser
               , handlePath "_upload"    GET  (ifLoggedIn "" uploadForm)
               , handlePath "_upload"    POST (ifLoggedIn "" uploadFile)
               , withCommand "showraw" [ handlePage GET showRawPage ]
               , withCommand "history" [ handlePage GET showPageHistory,
                                         handle (not . isPage) GET showFileHistory ]
               , withCommand "edit"    [ handlePage GET $ unlessNoEdit $ ifLoggedIn "?edit" editPage ]
               , withCommand "diff"    [ handlePage GET  showPageDiff,
                                         handle isSourceCode GET showFileDiff ]
               , withCommand "export"  [ handlePage POST exportPage, handlePage GET exportPage ]
               , withCommand "cancel"  [ handlePage POST showPage ]
               , withCommand "update"  [ handlePage POST  $ unlessNoEdit $ ifLoggedIn "?edit" updatePage ]
               , withCommand "delete"  [ handlePage GET  $ unlessNoDelete $ ifLoggedIn "?delete" confirmDelete,
                                         handlePage POST $ unlessNoDelete $ ifLoggedIn "?delete" deletePage ]
               , handleSourceCode
               , handleAny
               , handlePage GET showPage
               ]

data Params = Params { pUsername     :: String
                     , pPassword     :: String
                     , pPassword2    :: String
                     , pRevision     :: String
                     , pDestination  :: String
                     , pForUser      :: String
                     , pSince        :: String
                     , pRaw          :: String
                     , pLimit        :: Int
                     , pPatterns     :: [String]
                     , pEditedText   :: Maybe String
                     , pMessages     :: [String]
                     , pFrom         :: String
                     , pTo           :: String
                     , pFormat       :: String
                     , pSHA1         :: String
                     , pLogMsg       :: String
                     , pEmail        :: String
                     , pFullName     :: String
                     , pAccessCode   :: String
                     , pWikiname     :: String
                     , pOverwrite    :: Bool
                     , pFilename     :: String
                     , pFileContents :: B.ByteString
                     , pUser         :: String
                     , pConfirm      :: Bool 
                     , pSessionKey   :: Maybe SessionKey
                     }  deriving Show

instance FromData Params where
     fromData = do
         un <- look "username"       `mplus` return ""
         pw <- look "password"       `mplus` return ""
         p2 <- look "password2"      `mplus` return ""
         rv <- look "revision"       `mplus` return "HEAD"
         fu <- look "forUser"        `mplus` return ""
         si <- look "since"          `mplus` return ""
         ds <- look "destination"    `mplus` return ""
         ra <- look "raw"            `mplus` return ""
         lt <- look "limit"          `mplus` return "100"
         pa <- look "patterns"       `mplus` return ""
         me <- lookRead "messages"   `mplus` return [] 
         fm <- look "from"           `mplus` return "HEAD"
         to <- look "to"             `mplus` return "HEAD"
         et <- (look "editedText" >>= return . Just . filter (/= '\r')) `mplus` return Nothing
         fo <- look "format"         `mplus` return ""
         sh <- look "sha1"           `mplus` return ""
         lm <- look "logMsg"         `mplus` return ""
         em <- look "email"          `mplus` return ""
         na <- look "fullname"       `mplus` return ""
         wn <- look "wikiname"       `mplus` return ""
         ow <- (look "overwrite" >>= return . (== "yes")) `mplus` return False
         fn <- (lookInput "file" >>= return . fromMaybe "" . inputFilename) `mplus` return ""
         fc <- (lookInput "file" >>= return . inputValue) `mplus` return B.empty
         ac <- look "accessCode"     `mplus` return ""
         cn <- (look "confirm" >> return True) `mplus` return False
         sk <- (readCookieValue "sid" >>= return . Just) `mplus` return Nothing
         return $ Params { pUsername     = un
                         , pPassword     = pw
                         , pPassword2    = p2
                         , pRevision     = rv
                         , pForUser      = fu
                         , pSince        = si
                         , pDestination  = ds
                         , pRaw          = ra
                         , pLimit        = read lt
                         , pPatterns     = words pa
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
                         , pOverwrite    = ow
                         , pFilename     = fn
                         , pFileContents = fc
                         , pAccessCode   = ac
                         , pUser         = ""  -- this gets set by ifLoggedIn...
                         , pConfirm      = cn
                         , pSessionKey   = sk }

getLoggedInUser :: MonadIO m => Params -> m (Maybe String)
getLoggedInUser params = do
  mbSd <- maybe (return Nothing) ( query . GetSession ) $ pSessionKey params
  let user = case mbSd of
       Nothing    -> Nothing
       Just sd    -> Just $ sessionUser sd
  return $! user

data Command = Command (Maybe String)

commandList :: [String]
commandList = ["edit", "showraw", "history", "export", "diff", "cancel", "update", "delete"]

instance FromData Command where
     fromData = do
       pairs <- lookPairs
       return $ case (map fst pairs) `intersect` commandList of
                 []          -> Command Nothing
                 (c:_)       -> Command $ Just c

unlessNoEdit :: (String -> Params -> Web Response) -> (String -> Params -> Web Response)
unlessNoEdit responder =
  \page params -> do cfg <- query GetConfig
                     if page `elem` noEdit cfg
                        then showPage page (params { pMessages = ("Page is locked." : pMessages params) })
                        else responder page params

unlessNoDelete :: (String -> Params -> Web Response) -> (String -> Params -> Web Response)
unlessNoDelete responder =
  \page params ->  do cfg <- query GetConfig
                      if page `elem` noDelete cfg
                         then showPage page (params { pMessages = ("Page cannot be deleted." : pMessages params) })
                         else responder page params

ifLoggedIn :: String -> (String -> Params -> Web Response) -> (String -> Params -> Web Response)
ifLoggedIn fallback responder =
  \page params -> do user <- getLoggedInUser params
                     case user of
                          Nothing  -> seeOther ("/_login?" ++ urlEncodeVars [("destination", page ++ fallback)]) $ 
                                        toResponse $ p << "You must be logged in to perform this action."
                          Just u   -> do
                             usrs <- query AskUsers
                             let e = case M.lookup u usrs of
                                           Just usr    -> uEmail usr
                                           Nothing     -> error $ "User '" ++ u ++ "' not found."
                             responder page (params { pUser = u, pEmail = e })

handle :: (String -> Bool) -> Method -> (String -> Params -> Web Response) -> Handler
handle pathtest meth responder =
  uriRest $ \uri -> let path' = uriPath uri
                    in  if pathtest path'
                           then withData $ \params ->
                                    [ withRequest $ \req -> if rqMethod req == meth
                                                               then responder path' params
                                                               else noHandle ]
                           else anyRequest noHandle

-- | Returns path portion of URI, without initial /.
-- Consecutive spaces are collapsed.  We don't want to distinguish 'Hi There' and 'Hi  There'.
uriPath :: String -> String
uriPath = unwords . words . drop 1 . takeWhile (/='?')

handlePage :: Method -> (String -> Params -> Web Response) -> Handler
handlePage = handle isPage

handleText :: Method -> (String -> Params -> Web Response) -> Handler
handleText = handle (\x -> isPage x || isSourceCode x)

handlePath :: String -> Method -> (String -> Params -> Web Response) -> Handler
handlePath path' = handle (== path')

withCommand :: String -> [Handler] -> Handler
withCommand command handlers =
  withData $ \com -> case com of
                          Command (Just c) | c == command -> handlers
                          _                               -> []

handleSourceCode :: Handler
handleSourceCode = withData $ \com ->
  case com of
       Command (Just "showraw") -> [ handle isSourceCode GET showFileAsText ]
       _                        -> [ handle isSourceCode GET showHighlightedSource ]

handleAny :: Handler
handleAny = 
  uriRest $ \uri -> let path' = uriPath uri
                    in  do cfg <- query GetConfig
                           let file = repositoryPath cfg </> path'
                           exists <- liftIO $ doesFileExist file
                           if exists
                              then fileServe [path'] (repositoryPath cfg)
                              else anyRequest noHandle

orIfNull :: String -> String -> String
orIfNull str backup = if null str then backup else str

isPage :: String -> Bool
isPage ('_':_) = False
isPage s = '.' `notElem` s

isSourceCode :: String -> Bool
isSourceCode = not . null . languagesByExtension . takeExtension

urlForPage :: String -> String
urlForPage page = "/" ++ (substitute "%2f" "/" $ urlEncode page)
-- this is needed so that browsers recognize relative URLs correctly

pathForPage :: String -> FilePath
pathForPage page = page <.> "page"

withCommands :: Method -> [String] -> (String -> Request -> Web Response) -> Handler
withCommands meth commands page = withRequest $ \req -> do
  if rqMethod req /= meth
     then noHandle
     else if all (`elem` (map fst $ rqInputs req)) commands
             then page (intercalate "/" $ rqPaths req) req
             else noHandle

setContentType :: String -> Response -> Response
setContentType contentType res =
  let respHeaders = rsHeaders res
      newContentType = HeaderPair { hName = fromString "Content-Type",
                                    hValue = [ fromString contentType ] }
  in  res { rsHeaders = M.insert (fromString "content-type") newContentType respHeaders }  

setFilename :: String -> Response -> Response
setFilename fname res =
  let respHeaders = rsHeaders res
      newContentType = HeaderPair { hName = fromString "Content-Disposition",
                                    hValue = [ fromString $ "attachment; filename=\"" ++ fname ++ "\"" ] }
  in  res { rsHeaders = M.insert (fromString "content-disposition") newContentType respHeaders }  

showRawPage :: String -> Params -> Web Response
showRawPage page = showFileAsText $ pathForPage page

showFileAsText :: String -> Params -> Web Response
showFileAsText file params = do
  mContents <- rawContents file params
  case mContents of
       Nothing   -> error "Unable to retrieve page contents."
       Just c    -> ok $ setContentType "text/plain; charset=utf-8" $ toResponse c

showPage :: String -> Params -> Web Response
showPage "" params = do
  cfg <- query GetConfig
  showPage (frontPage cfg) params
showPage page params = do
  let revision = pRevision params
  mDoc <- pageAsPandoc page params
  case mDoc of
       Just d -> do
                 cont <- pandocToHtml d
                 let cont' = thediv ! [identifier "wikipage",
                                       strAttr "onDblClick" ("window.location = '" ++ urlForPage page ++ 
                                         "?edit&revision=" ++ revision ++
                                         (if revision == "HEAD"
                                             then ""
                                             else "&" ++ urlEncodeVars [("logMsg", "Revert to " ++ revision)]) ++ "';")] << cont
                 formattedPage [] ["jsMath/easy/load.js"] page params cont'
       _      -> if revision == "HEAD"
                    then (unlessNoEdit $ ifLoggedIn "" $ editPage) page params
                    else error $ "Invalid revision: " ++ revision

validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

uploadForm :: String -> Params -> Web Response
uploadForm _ params = do
  let page = "_upload"
  let origPath = pFilename params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  let upForm = form ! [X.method "post", enctype "multipart/form-data"] <<
        [ p << [label << "File to upload:", br, afile "file" ! [value origPath] ]
        , p << [label << "Name on wiki, including extension",
                noscript << " (leave blank to use the same filename)", stringToHtml ":", br,
                textfield "wikiname" ! [value wikiname],
                primHtmlChar "nbsp", checkbox "overwrite" "yes", label << "Overwrite existing file"]
        , p << [label << "Description of content or changes:", br, textfield "logMsg" ! [size "60", value logMsg],
                submit "upload" "Upload"] ]
  formattedPage [HidePageControls] ["uploadForm.js"] page params upForm

uploadFile :: String -> Params -> Web Response
uploadFile _ params = do
  let page = "_upload"
  let origPath = pFilename params
  let fileContents = pFileContents params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- query GetConfig
  let author = pUser params
  when (null author) $ fail "User must be logged in to upload a file."
  let email = pEmail params
  let overwrite = pOverwrite params
  exists <- liftIO $ doesFileExist (repositoryPath cfg </> wikiname)
  -- these are the dangerous extensions in HAppS-Server.Server.HTTP.FileServe.mimeMap.
  -- this list should be updated if mimeMap changes:
  let imageExtensions = [".png", ".jpg", ".gif"]
  let errors = validate [ (null logMsg, "Description cannot be empty.")
                        , (null origPath, "File not found.")
                        , (not overwrite && exists, "A file named '" ++ wikiname ++
                           "' already exists in the repository: choose a new name " ++
                           "or check the box to overwrite the existing file existing file.")
                        , (B.length fileContents > fromIntegral (maxUploadSize cfg),
                           "File exceeds maximum upload size.")
                        , (isPage wikiname,
                           "This file extension is reserved for wiki pages.")
                        ]
  if null errors
     then do
       if B.length fileContents > fromIntegral (maxUploadSize cfg)
          then error "File exceeds maximum upload size"
          else return ()
       let dir' = takeDirectory wikiname
       liftIO $ createDirectoryIfMissing True ((repositoryPath cfg) </> dir')
       liftIO $ B.writeFile ((repositoryPath cfg) </> wikiname) fileContents
       gitCommit wikiname (author, email) logMsg
       formattedPage [HidePageControls] [] page params $
                     thediv << [ h2 << ("Uploaded " ++ show (B.length fileContents) ++ " bytes")
                               , if takeExtension wikiname `elem` imageExtensions
                                    then p << "To add this image to a page, use:" +++
                                         pre << ("![alt text](/" ++ wikiname ++ ")")
                                    else p << "To link to this resource from a page, use:" +++
                                         pre << ("[link label](/" ++ wikiname ++ ")") ]
     else uploadForm page (params { pMessages = errors })

searchResults :: String -> Params -> Web Response
searchResults _ params = do
  let page = "_search"
  let patterns = pPatterns params
  let limit = pLimit params
  matchLines <- if null patterns
                   then return []
                   else gitGrep patterns >>= return . map parseMatchLine . take limit . lines
  let matchedFiles = nub $ filter (".page" `isSuffixOf`) $ map fst matchLines
  let matches = map (\f -> (f, mapMaybe (\(a,b) -> if a == f then Just b else Nothing) matchLines)) matchedFiles
  let preamble = if null matches
                    then h3 << if null patterns
                                  then ["Please enter a search term."]
                                  else ["No matches found for '", unwords patterns, "':"]
                    else h3 << [(show $ length matches), " matches found for '", unwords patterns, "':"]
  let htmlMatches = preamble +++ olist << map
                      (\(file, contents) -> li << [anchor ! [href $ urlForPage $ takeBaseName file] << takeBaseName file,
                      stringToHtml (" (" ++ show (length contents) ++ " matching lines)"),
                      stringToHtml " ", anchor ! [href "#", theclass "showmatch", thestyle "display: none;"] << "[show matches]",
                      pre ! [theclass "matches"] << unlines contents])
                      (reverse  $ sortBy (comparing (length . snd)) matches)
  formattedPage [HidePageControls] ["search.js"] page params htmlMatches

-- Auxiliary function for searchResults
parseMatchLine :: String -> (String, String)
parseMatchLine matchLine =
  let (file, rest) = break (==':') matchLine
      contents = drop 1 rest -- strip off colon
  in  (file, contents)

preview :: String -> Params -> Web Response
preview _ params = pandocToHtml (textToPandoc $ pRaw params) >>= ok . toResponse

showPageHistory :: String -> Params -> Web Response
showPageHistory page params = showHistory (pathForPage page) page params

showFileHistory :: String -> Params -> Web Response
showFileHistory file params = showHistory file file params

showHistory :: String -> String -> Params -> Web Response
showHistory file page params =  do
  let since = pSince params `orIfNull` "1 year ago"
  hist <- gitLog since "" [file]
  if null hist
     then noHandle
     else do
       let versionToHtml entry pos = 
              li ! [theclass "difflink", intAttr "order" pos, strAttr "revision" $ logRevision entry] <<
                   [thespan ! [theclass "date"] << logDate entry, stringToHtml " (",
                    thespan ! [theclass "author"] <<
                            anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", logAuthor entry)]] <<
                                       (logAuthor entry), stringToHtml ")", stringToHtml ": ",
                    anchor ! [href (urlForPage page ++ "?revision=" ++ logRevision entry)] <<
                    thespan ! [theclass "subject"] <<  logSubject entry,
                    noscript << ([stringToHtml " [compare with ",
                    anchor ! [href $ urlForPage page ++ "?diff&from=" ++ logRevision entry ++
                              "^&to=" ++ logRevision entry] << "previous"] ++
                                 (if pos /= 1
                                     then [primHtmlChar "nbsp", primHtmlChar "bull",
                                           primHtmlChar "nbsp",
                                           anchor ! [href $ urlForPage page ++ "?diff&from=" ++
                                                     logRevision entry ++ "&to=HEAD"] << "current" ]
                                     else []) ++
                                 [stringToHtml "]"])]
       let contents = ulist ! [theclass "history"] << zipWith versionToHtml hist [(length hist), (length hist - 1)..1]
       formattedPage [] ["dragdiff.js"] page params contents

showActivity :: String -> Params -> Web Response
showActivity _ params = do
  let page = "_activity"
  let since = pSince params `orIfNull` "1 month ago"
  let forUser = pForUser params
  hist <- gitLog since forUser []
  let filesFor files  = intersperse (primHtmlChar "nbsp") $ map
                           (\file -> anchor ! [href $ urlForPage file ++ "?history"] << file) $ map
                           (\file -> if ".page" `isSuffixOf` file then dropExtension file else file) files
  let heading = h1 << ("Recent changes" ++ if null forUser then "" else (" by " ++ forUser))
  let contents = ulist ! [theclass "history"] << map (\entry -> li <<
                           [thespan ! [theclass "date"] << logDate entry, stringToHtml " (",
                            thespan ! [theclass "author"] <<
                                    anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", logAuthor entry)]] <<
                                               (logAuthor entry), stringToHtml "):",
                            thespan ! [theclass "subject"] << logSubject entry, stringToHtml " (",
                            thespan ! [theclass "files"] << filesFor (logFiles entry),
                            stringToHtml ")"]) hist
  formattedPage [HidePageControls] [] page params (heading +++ contents)

showPageDiff :: String -> Params -> Web Response
showPageDiff page params = showDiff (pathForPage page) page params

showFileDiff :: String -> Params -> Web Response
showFileDiff page params = showDiff page page params

showDiff :: String -> String -> Params -> Web Response
showDiff file page params = do
  let from = pFrom params
  let to = pTo params
  rawDiff <- gitDiff file from to
  let diffLineToHtml l = case head l of
                                '+'   -> thespan ! [theclass "added"] << [tail l, "\n"]
                                '-'   -> thespan ! [theclass "deleted"] << [tail l, "\n"]
                                _     -> thespan << [tail l, "\n"]
  let formattedDiff = thespan ! [theclass "detail"] << ("Changes from " ++ from) +++
                      pre ! [theclass "diff"] << map diffLineToHtml (drop 5 $ lines rawDiff)
  formattedPage [] [] page (params { pRevision = to }) formattedDiff

editPage :: String -> Params -> Web Response
editPage page params = do
  let revision = pRevision params
  let messages = pMessages params
  raw <- case pEditedText params of
              Nothing -> gitCatFile revision (pathForPage page)
              Just t  -> return $ Just t
  let (new, contents) = case raw of
                             Nothing -> (True, "# Title goes here\n\nContent goes here")
                             Just c  -> (False, c)
  let messages' = if new
                     then ("This page does not yet exist.  You may create it by editing the text below." : messages)
                     else messages
  sha1 <- case (pSHA1 params) of
               ""  -> gitGetSHA1 (pathForPage page) >>= return . fromMaybe ""
               s   -> return s
  let logMsg = pLogMsg params
  let sha1Box = textfield "sha1" ! [thestyle "display: none", value sha1]
  let editForm = gui (urlForPage page) ! [identifier "editform"] <<
                   [sha1Box,
                    textarea ! [cols "80", name "editedText", identifier "editedText"] << contents,
                    label << "Description of changes:", br,
                    textfield "logMsg" ! [size "76", value logMsg],
                    submit "update" "Save", primHtmlChar "nbsp",
                    submit "cancel" "Discard", br,
                    thediv ! [ identifier "previewpane" ] << noHtml ]
  formattedPage [HidePageControls, HideNavbar] ["preview.js"] page (params {pMessages = messages'}) editForm

confirmDelete :: String -> Params -> Web Response
confirmDelete page params = do
  let confirmForm = gui "" <<
        [ p << "Are you sure you want to delete this page?"
        , submit "confirm" "Yes, delete it!"
        , stringToHtml " "
        , submit "cancel" "No, keep it!"
        , br ]
  formattedPage [HidePageControls] [] page params confirmForm

deletePage :: String -> Params -> Web Response
deletePage page params = do
  if pConfirm params
     then do
       let author = pUser params
       when (null author) $ fail "User must be logged in to delete page."
       let email = pEmail params
       gitRemove (pathForPage page) (author, email) "Deleted from web."
       seeOther "/" $ toResponse $ p << "Page deleted"
     else seeOther (urlForPage page) $ toResponse $ p << "Page not deleted"

updatePage :: String -> Params -> Web Response
updatePage page params = do
  let author = pUser params
  when (null author) $ fail "User must be logged in to update page."
  let editedText = case pEditedText params of
                      Nothing -> error "No body text in POST request"
                      Just b  -> b
  let email = pEmail params
  let logMsg = pLogMsg params
  let oldSHA1 = pSHA1 params
  if null logMsg
     then editPage page (params { pMessages = ["Description cannot be empty."] })
     else do
       cfg <- query GetConfig
       if length editedText > fromIntegral (maxUploadSize cfg)
          then error "Page exceeds maximum size."
          else return ()
       currentSHA1 <- gitGetSHA1 (pathForPage page) >>= return . fromMaybe ""
       -- ensure that every file has a newline at the end, to avoid "No newline at eof" messages in diffs
       let editedText' = if null editedText || last editedText == '\n' then editedText else editedText ++ "\n"
       -- check SHA1 in case page has been modified, merge
       if currentSHA1 == oldSHA1
          then do
            let dir' = takeDirectory page
            liftIO $ createDirectoryIfMissing True ((repositoryPath cfg) </> dir')
            liftIO $ writeFile ((repositoryPath cfg) </> pathForPage page) editedText'
            gitCommit (pathForPage page) (author, email) logMsg
            seeOther (urlForPage page) $ toResponse $ p << "Page updated"
          else do -- there have been conflicting changes
            original <- gitCatFile oldSHA1 (pathForPage page) >>= return . fromJust
            latest <- gitCatFile currentSHA1 (pathForPage page) >>= return . fromJust
            let pagePath = repositoryPath cfg </> pathForPage page
            let [textTmp, originalTmp, latestTmp] = map (pagePath ++) [".edited",".original",".latest"]
            liftIO $ writeFile textTmp editedText'
            liftIO $ writeFile originalTmp original
            liftIO $ writeFile latestTmp latest
            mergeText <- gitMergeFile (pathForPage page ++ ".edited") (pathForPage page ++ ".original") (pathForPage page ++ ".latest")
            liftIO $ mapM removeFile [textTmp, originalTmp, latestTmp]
            let mergeMsg = "The page has been edited since you checked it out. " ++
                           "Changes have been merged into your edits below. " ++
                           "Please resolve conflicts and Save."
            editPage page (params { pEditedText = Just mergeText
                                  , pRevision = "HEAD"
                                  , pSHA1 = currentSHA1
                                  , pMessages = [mergeMsg] })

indexPage :: String -> Params -> Web Response
indexPage _ params = do
  let page = "_index"
  let revision = pRevision params
  files <- gitLsTree revision >>= return . map (unwords . drop 3 . words) . lines
  let htmlIndex = fileListToHtml "/" $ map splitPath $ sort files
  formattedPage [HidePageControls] ["folding.js"] page params htmlIndex

-- | Map a list of nonempty lists onto a list of pairs of list heads and list of tails.
-- e.g. [[1,2],[1],[2,1]] -> [(1,[[2],[]]), (2,[[1]])]
consolidateHeads :: Eq a => [[a]] -> [(a,[[a]])]
consolidateHeads lst =
  let heads = nub $ map head lst
      tailsFor h = map tail [l | l <- lst, head l == h]
  in  map (\h -> (h, tailsFor h)) heads

-- | Create a hierarchical ordered list (with links) for a list of files
fileListToHtml :: String -> [[FilePath]] -> Html
fileListToHtml prefix lst = ulist ! [identifier "index", theclass "folding"] <<
  (map (\(h, l) -> let h' = if ".page" `isSuffixOf` h then dropExtension h else h
                   in if [] `elem` l
                         then li ! [theclass $ if isPage h' then "page" else "upload"] << anchor ! [href $ prefix ++ h'] << h'
                         else li ! [theclass "folder"] << [stringToHtml h', fileListToHtml (prefix ++ h') l]) $
  consolidateHeads lst)

-- | Convert links with no URL to wikilinks, if their labels are all strings and spaces
convertWikiLinks :: Inline -> Inline
convertWikiLinks (Link ref ("", "")) | all isStringOrSpace ref =
  Link ref (refToUrl ref, "Go to wiki page")
convertWikiLinks x = x

isStringOrSpace :: Inline -> Bool
isStringOrSpace Space   = True
isStringOrSpace (Str _) = True
isStringOrSpace _       = False

refToUrl :: [Inline] -> String
refToUrl ((Str x):xs) = x ++ refToUrl xs
refToUrl (Space:xs)   = "%20" ++ refToUrl xs
refToUrl (_:_)        = error "Encountered an inline other than Str or Space"
refToUrl []           = ""

-- | Converts pandoc document to HTML.
pandocToHtml :: MonadIO m => Pandoc -> m Html
pandocToHtml pandocContents = do
  cfg <- query GetConfig
  return $ writeHtml (defaultWriterOptions { writerStandalone = False
                                           , writerHTMLMathMethod = JsMath (Just "/javascripts/jsMath/easy/load.js")
                                           , writerTableOfContents = tableOfContents cfg
                                           }) $ processPandoc convertWikiLinks pandocContents

data PageOption = HidePageControls | HideNavbar deriving (Eq, Show)

-- | Returns formatted page
formattedPage :: [PageOption] -> [String] -> String -> Params -> Html -> Web Response
formattedPage opts scripts page params htmlContents = do
  let revision = pRevision params
  user <- getLoggedInUser params
  cfg <- (query GetConfig)
  let stylesheetlinks = thelink ! [href "/stylesheets/gitit.css", rel "stylesheet",
                                   strAttr "media" "screen", thetype "text/css"] << noHtml +++
                        thelink ! [href "/stylesheets/hk-pyg.css", rel "stylesheet",
                                   strAttr "media" "screen", thetype "text/css"] << noHtml +++
                        thelink ! [href "/stylesheets/gitit-print.css", rel "stylesheet",
                                   strAttr "media" "print", thetype "text/css"] << noHtml
  let javascriptlinks = if null scripts
                           then noHtml
                           else concatHtml $ map
                                  (\x -> script ! [src ("/javascripts/" ++ x), thetype "text/javascript"] << noHtml)
                                  (["jquery.min.js", "jquery-ui-personalized-1.6rc2.min.js"] ++ scripts)
  let title' = thetitle << (wikiTitle cfg ++ " - " ++ dropWhile (=='_') page)
  let head' = header << [title', stylesheetlinks, javascriptlinks]
  let sitenav = thediv ! [theclass "sitenav"] <<
                        gui ("/_search") ! [identifier "searchform"] <<
                          (intersperse (primHtmlChar "bull")
                             [ anchor ! [href "/", theclass "nav_link"] << "front"
                             , anchor ! [href "/_index", theclass "nav_link"] << "index"
                             , anchor ! [href "/_upload", theclass "nav_link"] << "upload"
                             , anchor ! [href "/_activity", theclass "nav_link"] << "activity"
                             , anchor ! [href "/Help", theclass "nav_link"] << "help" ] ++
                          [ primHtmlChar "nbsp"
                          , textfield "patterns" ! [theclass "search_field search_term"]
                          , submit "search" "Search" ])
  let rawButton  = anchor ! [href $ urlForPage page ++ "?revision=" ++ revision ++ "&showraw", theclass "nav_link"] << "raw"
  let delButton  = anchor ! [href $ urlForPage page ++ "?delete", theclass "nav_link"] << "delete"
  let histButton = anchor ! [href $ urlForPage page ++ "?revision=" ++ revision ++ "&history", theclass "nav_link"] << "history"
  let editButton = anchor ! [href $ urlForPage page ++ "?edit&revision=" ++ revision ++
                              if revision == "HEAD" then "" else "&" ++ urlEncodeVars [("logMsg", "Revert to " ++ revision)],
                              theclass "nav_link"] << if revision == "HEAD" then "edit" else "revert"
  let buttons    = intersperse (primHtmlChar "bull") $ 
                      (if isPage page then [rawButton] else []) ++ [delButton, histButton] ++
                      (if isPage page then [editButton] else [])
  let userbox =    thediv ! [identifier "userbox"] <<
                        case user of
                             Just u   -> anchor ! [href ("/_logout?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] << 
                                         ("logout " ++ u)
                             Nothing  -> (anchor ! [href ("/_login?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] <<
                                          "login") +++ primHtmlChar "bull" +++
                                         anchor ! [href ("/_register?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] <<
                                          "register"
  let sitenavVis = if HideNavbar `elem` opts then "hidden" else "visible"
  let pageinfoVis = if HidePageControls `elem` opts then "none" else "show"
  let messages = pMessages params
  let htmlMessages = if null messages
                        then noHtml
                        else ulist ! [theclass "messages"] << map (li <<) messages
  let body' = body << thediv ! [identifier "container"] <<
                        [ thediv ! [identifier "banner"] << primHtml (wikiBanner cfg)
                        , thediv ! [identifier "navbar", thestyle $ "visibility: " ++ sitenavVis] << [userbox, sitenav]
                        , case page of
                               '_':_   -> noHtml
                               _       -> thediv ! [identifier "pageTitle"] << [ anchor ! [href $ urlForPage page] << (h1 << page) ]
                        , thediv ! [identifier "content"] << [htmlMessages, htmlContents]
                        , thediv ! [identifier "pageinfo", thestyle $ "display: " ++ pageinfoVis] <<
                                        [ thediv ! [theclass "pageControls"] <<
                                                                            ((thespan ! [theclass "details"] << revision) : buttons)
                                                              , if isPage page then exportBox page params else noHtml]
                        , thediv ! [identifier "footer"] << primHtml (wikiFooter cfg)
                        ]
  ok $ toResponse $ head' +++ body'

-- user authentication
loginForm :: Params -> Html
loginForm params =
  let destination = pDestination params
  in  gui "" ! [identifier "loginForm"] <<
             [ textfield "sha1" ! [thestyle "display: none", value destination]
             , label << "Username ", textfield "username" ! [size "15"], stringToHtml " "
             , label << "Password ", X.password "password" ! [size "15"], stringToHtml " "
             , submit "login" "Login"
             , p << [ stringToHtml "If you do not have an account, "
                    , anchor ! [href ("/_register?" ++ urlEncodeVars [("destination", destination)])] << "click here to register." ]]

loginUserForm :: String -> Params -> Web Response
loginUserForm _ params = formattedPage [HidePageControls] [] "_login" params $ loginForm params

loginUser :: String -> Params -> Web Response
loginUser _ params = do
  let uname = pUsername params
  let pword = pPassword params
  let destination = pDestination params
  cfg <- query GetConfig
  let passwordHash = SHA512.hash $ map c2w $ passwordSalt cfg ++ pword
  allowed <- query $ AuthUser uname passwordHash
  if allowed
    then do
      key <- update $ NewSession (SessionData uname)
      addCookie (3600) (mkCookie "sid" (show key))
      seeOther ("/" ++ substitute " " "%20" destination) $ toResponse $ p << ("Welcome, " ++ uname)
    else
      loginUserForm "_login" (params { pMessages = "Authentication failed." : pMessages params })

logoutUser :: String -> Params -> Web Response
logoutUser _ params = do
  let key = pSessionKey params
  let destination = pDestination params
  case key of
       Just k  -> update $ DelSession k
       Nothing -> return ()
  seeOther ("/" ++ substitute " " "%20" destination) $ toResponse $ p << "You have been logged out."

registerForm :: Web Html
registerForm = do
  cfg <- query GetConfig
  let accessQ = case accessQuestion cfg of
        Nothing          -> noHtml
        Just (prompt, _) -> label << prompt +++ br +++
                            X.password "accessCode" ! [size "15"] +++ br
  return $ gui "" ! [identifier "loginForm"] <<
            [ accessQ,
              label << "Username (at least 3 letters or digits):", br,
              textfield "username" ! [size "20"], stringToHtml " ", br,
              label << "Email (optional, will not be displayed on the Wiki):", br,
              textfield "email" ! [size "20"], br,
              textfield "fullname" ! [size "20", theclass "req"],
              label << "Password (at least 6 characters, including at least one non-letter):", br,
              X.password "password" ! [size "20"], stringToHtml " ", br,
              label << "Confirm Password:", br, X.password "password2" ! [size "20"], stringToHtml " ", br,
              submit "register" "Register" ]

registerUserForm :: String -> Params -> Web Response
registerUserForm _ params = do
  let page = "_register"
  regForm <- registerForm
  formattedPage [HidePageControls] [] page params regForm

registerUser :: String -> Params -> Web Response
registerUser _ params = do
  let page = "_register"
  regForm <- registerForm
  let isValidUsername u = length u >= 3 && all isAlphaNum u
  let isValidPassword pw = length pw >= 6 && not (all isAlpha pw)
  let accessCode = pAccessCode params
  let uname = pUsername params
  let pword = pPassword params
  let pword2 = pPassword2 params
  let email = pEmail params
  let fakeField = pFullName params
  taken <- query $ IsUser uname
  cfg <- query GetConfig
  let isValidAccessCode = case accessQuestion cfg of
        Nothing           -> True
        Just (_, answers) -> accessCode `elem` answers
  let isValidEmail e = length (filter (=='@') e) == 1
  let errors = validate [ (taken, "Sorry, that username is already taken.")
                        , (not isValidAccessCode, "Incorrect response to access prompt.")
                        , (not (isValidUsername uname), "Username must be at least 3 charcaters, all letters or digits.")
                        , (not (isValidPassword pword), "Password must be at least 6 characters, with at least one non-letter.")
                        , (not (isValidEmail email), "Email address appears invalid.")
                        , (pword /= pword2, "Password does not match confirmation.")
                        , (not (null fakeField), "You do not seem human enough.") ] -- fakeField is hidden in CSS (honeypot)
  if null errors
     then do
       let passwordHash = SHA512.hash $ map c2w $ passwordSalt cfg ++ pword
       update $ AddUser uname (User { uUsername = uname, uPassword = passwordHash, uEmail = email })
       loginUser "/" (params { pUsername = uname, pPassword = pword })
     else formattedPage [HidePageControls] [] page (params { pMessages = errors }) regForm

showHighlightedSource :: String -> Params -> Web Response
showHighlightedSource file params = do
  contents <- rawContents file params
  case contents of
      Just source -> let lang' = head $ languagesByExtension $ takeExtension file
                     in case highlightAs lang' (filter (/='\r') source) of
                              Left _       -> noHandle
                              Right res    -> formattedPage [] [] file params $ formatAsXHtml [OptNumberLines] lang' res
      Nothing     -> noHandle

defaultRespOptions :: WriterOptions
defaultRespOptions = defaultWriterOptions { writerStandalone = True, writerWrapText = True }

respondLaTeX :: String -> Pandoc -> Web Response
respondLaTeX page = ok . setContentType "application/x-latex" . setFilename (page ++ ".tex") . toResponse .
                    writeLaTeX (defaultRespOptions {writerHeader = defaultLaTeXHeader})

respondConTeXt :: String -> Pandoc -> Web Response
respondConTeXt page = ok . setContentType "application/x-context" . setFilename (page ++ ".tex") . toResponse .
                      writeConTeXt (defaultRespOptions {writerHeader = defaultConTeXtHeader})

respondRTF :: String -> Pandoc -> Web Response
respondRTF page = ok . setContentType "application/rtf" . setFilename (page ++ ".rtf") . toResponse .
                  writeRTF (defaultRespOptions {writerHeader = defaultRTFHeader})

respondRST :: String -> Pandoc -> Web Response
respondRST _ = ok . setContentType "text/plain" . toResponse .
               writeRST (defaultRespOptions {writerHeader = "", writerReferenceLinks = True})

respondMan :: String -> Pandoc -> Web Response
respondMan _ = ok . setContentType "text/plain" . toResponse .
               writeMan (defaultRespOptions {writerHeader = ""})

respondS5 :: String -> Pandoc -> Web Response
respondS5 _ = ok . toResponse .  writeS5 (defaultRespOptions {writerHeader = defaultS5Header, 
                                            writerS5 = True, writerIncremental = True})

respondTexinfo :: String -> Pandoc -> Web Response
respondTexinfo page = ok . setContentType "application/x-texinfo" . setFilename (page ++ ".texi") . toResponse .
                      writeTexinfo (defaultRespOptions {writerHeader = ""})

respondDocbook :: String -> Pandoc -> Web Response
respondDocbook page = ok . setContentType "application/docbook+xml" . setFilename (page ++ ".xml") . toResponse .
                      writeDocbook (defaultRespOptions {writerHeader = defaultDocbookHeader})

respondMediaWiki :: String -> Pandoc -> Web Response
respondMediaWiki _ = ok . setContentType "text/plain" . toResponse .
                     writeMediaWiki (defaultRespOptions {writerHeader = ""})

respondODT :: String -> Pandoc -> Web Response
respondODT page doc = do
  cfg <- query GetConfig
  let openDoc = writeOpenDocument (defaultRespOptions {writerHeader = defaultOpenDocumentHeader}) doc
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                saveOpenDocumentAsODT tempfile (repositoryPath cfg) openDoc
                B.readFile tempfile
  ok $ setContentType "application/vnd.oasis.opendocument.text" $ setFilename (page ++ ".odt") $ (toResponse noHtml) {rsBody = contents}

exportFormats :: [(String, String -> Pandoc -> Web Response)]   -- (description, writer)
exportFormats = [ ("LaTeX",     respondLaTeX)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("MediaWiki", respondMediaWiki)
                , ("man",       respondMan)
                , ("DocBook",   respondDocbook)
                , ("S5",        respondS5)
                , ("ODT",       respondODT)
                , ("RTF",       respondRTF) ]

exportBox :: String -> Params -> Html
exportBox page params =
   gui (urlForPage page) ! [identifier "exportbox"] << 
     [ submit "export" "Export"
     , textfield "revision" ! [thestyle "display: none;", value (pRevision params)]
     , select ! [name "format"] <<
         map ((\f -> option ! [value f] << ("as " ++ f)) . fst) exportFormats ]

rawContents :: String -> Params -> Web (Maybe String)
rawContents file params = do
  let revision = pRevision params `orIfNull` "HEAD"
  gitCatFile revision file

textToPandoc :: String -> Pandoc
textToPandoc = readMarkdown (defaultParserState { stateSanitizeHTML = True, stateSmart = True }) .
               filter (/= '\r') .  decodeString

pageAsPandoc :: String -> Params -> Web (Maybe Pandoc)
pageAsPandoc page params = do
  mDoc <- rawContents (pathForPage page) params >>= (return . liftM textToPandoc)
  return $ case mDoc of
           Nothing                -> Nothing
           Just (Pandoc _ blocks) -> Just $ Pandoc (Meta [Str page] [] []) blocks

exportPage :: String -> Params -> Web Response
exportPage page params = do
  let format = pFormat params
  mDoc <- pageAsPandoc page params
  case mDoc of
       Nothing  -> error $ "Unable to retrieve page contents."
       Just doc -> case lookup format exportFormats of
                        Nothing     -> error $ "Unknown export format: " ++ format
                        Just writer -> writer page doc

-- | Perform a function in a temporary directory and clean up.
withTempDir :: FilePath -> (FilePath -> IO a) -> IO a
withTempDir baseName = bracket (createTempDir 0 baseName) (removeDirectoryRecursive)

-- | Create a temporary directory with a unique name.
createTempDir :: Integer -> FilePath -> IO FilePath
createTempDir num baseName = do
  sysTempDir <- catch getTemporaryDirectory (\_ -> return ".")
  let dirName = sysTempDir </> baseName <.> show num
  catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
               then createTempDir (num + 1) baseName
               else ioError e

