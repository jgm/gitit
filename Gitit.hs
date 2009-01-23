{-# LANGUAGE Rank2Types, FlexibleContexts #-}
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

import HAppS.Server hiding (look, lookRead, lookCookieValue, mkCookie)
import Gitit.HAppS (look, lookRead, lookCookieValue, mkCookie)
import System.Environment
import System.IO.UTF8
import System.IO (stderr)
import System.IO.Error (isAlreadyExistsError)
import Control.Exception (bracket, throwIO, catch, try)
import Prelude hiding (writeFile, readFile, putStrLn, putStr, catch)
import System.Directory
import System.Time
import Control.Concurrent
import System.FilePath
import Gitit.State
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( password, method )
import Data.List (intersect, intersperse, intercalate, sort, nub, sortBy, isSuffixOf, find, isPrefixOf)
import Data.Maybe (fromMaybe, fromJust, mapMaybe, isJust, isNothing)
import Data.ByteString.UTF8 (fromString, toString)
import Codec.Binary.UTF8.String (decodeString, encodeString)
import qualified Data.Map as M
import Data.Ord (comparing)
import Paths_gitit
import Text.Pandoc
import Text.Pandoc.ODT (saveOpenDocumentAsODT)
import Text.Pandoc.Definition (processPandoc)
import Text.Pandoc.Shared (HTMLMathMethod(..), substitute)
import Data.Char (isAlphaNum, isAlpha, toLower)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Codec.Compression.GZip (compress)
import Network.HTTP (urlEncodeVars, urlEncode)
import System.Console.GetOpt
import System.Exit
import Text.Highlighting.Kate
import qualified Text.StringTemplate as T
import Data.IORef
import Data.DateTime (getCurrentTime, addMinutes, parseDateTime, DateTime)
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import Data.FileStore

gititVersion :: String
gititVersion = "0.4.1.3"

sessionTime :: Int
sessionTime = 60 * 60     -- session will expire 1 hour after page request

template :: IORef (T.StringTemplate String)
template = unsafePerformIO $ newIORef $ T.newSTMP ""  -- initialize template to empty string

main :: IO ()
main = do
  argv <- getArgs
  options <- parseArgs argv
  conf <- foldM handleFlag defaultConfig options
  -- check for external programs that are needed
  let prereqs = "grep" : case repository conf of
                      Git _        -> ["git"]
                      Darcs _      -> ["darcs"]
  forM_ prereqs $ \prog ->
    findExecutable prog >>= \mbFind ->
    when (isNothing mbFind) $ error $ "Required program '" ++ prog ++ "' not found in system path."
  -- read user file and update state
  userFileExists <- doesFileExist $ userFile conf
  users' <- if userFileExists
               then readFile (userFile conf) >>= (return . M.fromList . read)
               else return M.empty
  initializeAppState conf users'
  initializeWiki conf
  -- initialize template
  templ <- liftIO $ readFile (templateFile conf)
  writeIORef template (T.newSTMP templ)
  hPutStrLn stderr $ "Starting server on port " ++ show (portNumber conf)
  let debugger = if debugMode conf then debugFilter else id
  tid <- forkIO $ simpleHTTP (Conf { validator = Nothing, port = portNumber conf }) $
          debugger $
          map (filterIf acceptsZip gzipBinary) $
          [ dir "css" [ fileServe [] $ staticDir conf </> "css" ]
          , dir "img" [ fileServe [] $ staticDir conf </> "img" ]
          , dir "js"  [ fileServe [] $ staticDir conf </> "js" ]
          ] ++ (if debugMode conf then debugHandlers else []) ++ wikiHandlers
  waitForTermination
  putStrLn "Shutting down..."
  killThread tid
  putStrLn "Shutdown complete"

filterIf :: (Request -> Bool) -> (Response -> Response) -> ServerPart Response -> ServerPart Response
filterIf test filt sp =
  let handler = unServerPartT sp
  in  withRequest $ \req ->
      if test req
         then liftM filt $ handler req
         else handler req

gzipBinary :: Response -> Response
gzipBinary r@(Response {rsBody=b}) =  setHeader "Content-Encoding" "gzip" $ r {rsBody = compress b}

acceptsZip :: Request -> Bool
acceptsZip req =
  case M.lookup (fromString "accept-encoding") (rqHeaders req) of
        Just _ -> True
        _      -> False

setContentType :: String -> Response -> Response
setContentType = setHeader "Content-Type"

setFilename :: String -> Response -> Response
setFilename = setHeader "Content-Disposition" . \fname -> "attachment: filename=\"" ++ fname ++ "\""

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
    ConfigFile f -> liftM read (readFile f)

-- | Create repository and public directories, unless they already exist.
initializeWiki :: Config -> IO ()
initializeWiki conf = do
  let staticdir = staticDir conf
  let templatefile = templateFile conf
  fs <- getFileStore
  repoExists <- liftIO $ catch (initialize fs >> return False)
                               (\e -> if e == RepositoryExists then return True else throwIO e >> return False)
  unless repoExists $ do
    welcomepath <- getDataFileName $ "data" </> "FrontPage.page"
    welcomecontents <- B.readFile welcomepath
    helppath <- getDataFileName $ "data" </> "Help.page"
    helpcontents <- B.readFile helppath
    -- add front page and help page
    liftIO $ create fs (frontPage conf <.> "page") (Author "Gitit" "") "Default front page" welcomecontents
    liftIO $ create fs "Help.page" (Author "Gitit" "") "Default front page" helpcontents
    hPutStrLn stderr "Created repository"
  staticExists <- doesDirectoryExist staticdir
  unless staticExists $ do
    createDirectoryIfMissing True $ staticdir </> "css"
    let stylesheets = map ("css" </>) ["screen.css", "print.css", "ie.css", "hk-pyg.css"]
    stylesheetpaths <- mapM getDataFileName stylesheets
    zipWithM_ copyFile stylesheetpaths (map (staticdir </>) stylesheets)
    createDirectoryIfMissing True $ staticdir </> "img" </> "icons"
    let imgs = map (("img" </>) . ("icons" </>))
                ["cross.png", "doc.png", "email.png", "external.png", "feed.png", "folder.png",
                 "im.png", "key.png", "page.png", "pdf.png", "tick.png", "xls.png"]
    imgpaths <- mapM getDataFileName imgs
    zipWithM_ copyFile imgpaths (map (staticdir </>) imgs)
    logopath <- getDataFileName $ "img" </> "gitit-dog.png"
    copyFile logopath (staticdir </> "img" </> "logo.png")
    createDirectoryIfMissing True $ staticdir </> "js"
    let javascripts = ["jquery.min.js", "jquery-ui.packed.js",
                       "folding.js", "dragdiff.js", "preview.js", "search.js", "uploadForm.js"]
    javascriptpaths <- mapM getDataFileName $ map ("js" </>) javascripts
    zipWithM_ copyFile javascriptpaths $ map ((staticdir </> "js") </>) javascripts
    hPutStrLn stderr $ "Created " ++ staticdir ++ " directory"
  jsMathExists <- doesDirectoryExist (staticdir </> "js" </> "jsMath")
  updateAppState $ \s -> s{ jsMath = jsMathExists }
  unless jsMathExists $ do
    hPutStrLn stderr $ replicate 80 '*' ++
                       "\nWarning:  jsMath not found.\n" ++
                       "If you want support for math, copy the jsMath directory into " ++ staticdir ++ "/js/\n" ++
                       "jsMath can be obtained from http://www.math.union.edu/~dpvc/jsMath/\n" ++
                       replicate 80 '*'
  templateExists <- doesFileExist templatefile
  unless templateExists $ do
    templatePath <- getDataFileName $ "data" </> "template.html"
    copyFile templatePath templatefile
    hPutStrLn stderr $ "Created " ++ templatefile

type Handler = ServerPart Response


debugHandlers :: [Handler]
debugHandlers = [ withCommand "params"  [ handlePage GET  $ \_ params -> ok (toResponse $ show params),
                                          handlePage POST $ \_ params -> ok (toResponse $ show params) ]
                , withCommand "page"    [ handlePage GET  $ \page _ -> ok (toResponse $ show page),
                                          handlePage POST $ \page _ -> ok (toResponse $ show page) ]
                , withCommand "request" [ withRequest $ \req -> ok $ toResponse $ show req ] ]

wikiHandlers :: [Handler]
wikiHandlers = [ handlePath "_index"     GET  indexPage
               , handlePath "_activity"  GET  showActivity
               , handlePath "_preview"   POST preview
               , handlePath "_go"        POST goToPage
               , handlePath "_search"    POST searchResults
               , handlePath "_search"    GET  searchResults
               , handlePath "_register"  GET  registerUserForm
               , handlePath "_register"  POST registerUser
               , handlePath "_login"     GET  loginUserForm
               , handlePath "_login"     POST loginUser
               , handlePath "_logout"    GET  logoutUser
               , handlePath "_upload"    GET  (ifLoggedIn uploadForm)
               , handlePath "_upload"    POST (ifLoggedIn uploadFile)
               , handlePath "_random"    GET  randomPage
               , handlePath ""           GET  showFrontPage
               , withCommand "showraw" [ handlePage GET showRawPage ]
               , withCommand "history" [ handlePage GET showPageHistory,
                                         handle (not . isPage) GET showFileHistory ]
               , withCommand "edit"    [ handlePage GET $ unlessNoEdit $ ifLoggedIn editPage ]
               , withCommand "diff"    [ handlePage GET  showPageDiff,
                                         handle isSourceCode GET showFileDiff ]
               , withCommand "export"  [ handlePage POST exportPage, handlePage GET exportPage ]
               , withCommand "cancel"  [ handlePage POST showPage ]
               , withCommand "discuss" [ handlePage GET discussPage ]
               , withCommand "update"  [ handlePage POST $ unlessNoEdit $ ifLoggedIn updatePage ]
               , withCommand "delete"  [ handlePage GET  $ unlessNoDelete $ ifLoggedIn confirmDelete,
                                         handlePage POST $ unlessNoDelete $ ifLoggedIn deletePage ]
               , handleSourceCode
               , handleAny
               , handlePage GET showPage
               ]

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
                     , pForUser      :: String
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
                     , pIPAddress    :: String
                     }  deriving Show

instance FromData Params where
     fromData = do
         un <- look "username"       `mplus` return ""
         pw <- look "password"       `mplus` return ""
         p2 <- look "password2"      `mplus` return ""
         rv <- (look "revision" >>= \s ->
                 return (if null s then Nothing else Just s)) `mplus` return Nothing
         fu <- look "forUser"        `mplus` return ""
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
         na <- look "fullname"       `mplus` return ""
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
                         , pIPAddress    = ""  -- this gets set by handle...
                         }

getLoggedInUser :: MonadIO m => Params -> m (Maybe String)
getLoggedInUser params = do
  mbSd <- maybe (return Nothing) getSession $ pSessionKey params
  let user = case mbSd of
       Nothing    -> Nothing
       Just sd    -> Just $ sessionUser sd
  return $! user

data Command = Command (Maybe String)

commandList :: [String]
commandList = ["page", "request", "params", "edit", "showraw", "history", "export", "diff", "cancel", "update", "delete", "discuss"]

instance FromData Command where
     fromData = do
       pairs <- lookPairs
       return $ case map fst pairs `intersect` commandList of
                 []          -> Command Nothing
                 (c:_)       -> Command $ Just c

unlessNoEdit :: (String -> Params -> Web Response) -> (String -> Params -> Web Response)
unlessNoEdit responder =
  \page params -> do cfg <- getConfig
                     if page `elem` noEdit cfg
                        then showPage page (params { pMessages = ("Page is locked." : pMessages params) })
                        else responder page params

unlessNoDelete :: (String -> Params -> Web Response) -> (String -> Params -> Web Response)
unlessNoDelete responder =
  \page params ->  do cfg <- getConfig
                      if page `elem` noDelete cfg
                         then showPage page (params { pMessages = ("Page cannot be deleted." : pMessages params) })
                         else responder page params

ifLoggedIn :: (String -> Params -> Web Response) -> (String -> Params -> Web Response)
ifLoggedIn responder =
  \page params -> do user <- getLoggedInUser params
                     case user of
                          Nothing  -> do
                             loginUserForm page (params { pReferer = Just $ pUri params })
                          Just u   -> do
                             usrs <- queryAppState users
                             let e = case M.lookup u usrs of
                                           Just usr    -> uEmail usr
                                           Nothing     -> error $ "User '" ++ u ++ "' not found."
                             -- give the user another hour...
                             addCookie sessionTime (mkCookie "sid" (show $ fromJust $ pSessionKey params))
                             responder page (params { pUser = u, pEmail = e })

handle :: (String -> Bool) -> Method -> (String -> Params -> Web Response) -> Handler
handle pathtest meth responder = uriRest $ \uri ->
  let path' = decodeString $ uriPath uri
  in  if pathtest path'
         then withData $ \params ->
                  [ withRequest $ \req ->
                      if rqMethod req == meth
                         then do
                           let referer = case M.lookup (fromString "referer") (rqHeaders req) of
                                              Just r | not (null (hValue r)) -> Just $ toString $ head $ hValue r
                                              _       -> Nothing
                           let peer = fst $ rqPeer req
                           mbIPaddr <- liftIO $ lookupIPAddr peer
                           let ipaddr = case mbIPaddr of
                                             Just ip -> ip
                                             Nothing -> "0.0.0.0"
                           -- force ipaddr to be strictly evaluated, or we run into problems when validating captchas
                           ipaddr `seq` responder path' (params { pReferer = referer,
                                                                  pUri = uri,
                                                                  pIPAddress = ipaddr })
                         else noHandle ]
         else anyRequest noHandle

lookupIPAddr :: String -> IO (Maybe String)
lookupIPAddr hostname = do
  addrs <- getAddrInfo (Just defaultHints) (Just hostname) Nothing
  if null addrs
     then return Nothing
     else return $ Just $ takeWhile (/=':') $ show $ addrAddress $ head addrs

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
                    in  do fs <- getFileStore
                           mimetype <- getMimeTypeForExtension (takeExtension path')
                           res <- liftIO $ try $ (retrieve fs path' Nothing  :: IO B.ByteString)
                           case res of
                                  Right contents -> anyRequest $ ok $ setContentType mimetype $
                                                               (toResponse noHtml) {rsBody = contents} -- ugly hack
                                  Left NotFound  -> anyRequest noHandle
                                  Left e         -> error (show e)

orIfNull :: String -> String -> String
orIfNull str backup = if null str then backup else str

isPage :: String -> Bool
isPage ('_':_) = False
isPage s = '.' `notElem` s

isDiscussPage :: String -> Bool
isDiscussPage s = isPage s && ":discuss" `isSuffixOf` s

isSourceCode :: String -> Bool
isSourceCode = not . null . languagesByExtension . takeExtension

urlForPage :: String -> String
urlForPage page = '/' : (substitute "%2f" "/" $ urlEncode $ encodeString page)
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

showRawPage :: String -> Params -> Web Response
showRawPage = showFileAsText . pathForPage

showFileAsText :: String -> Params -> Web Response
showFileAsText file params = do
  mContents <- rawContents file params
  case mContents of
       Nothing   -> error "Unable to retrieve page contents."
       Just c    -> ok $ setContentType "text/plain; charset=utf-8" $ toResponse $ fromString c

randomPage :: String -> Params -> Web Response
randomPage _ _ = do
  fs <- getFileStore
  files <- liftIO $ index fs
  let pages = map dropExtension $ filter (\f -> takeExtension f == ".page" && not (":discuss.page" `isSuffixOf` f)) files
  if null pages
     then error "No pages found!"
     else do
       TOD _ picosecs <- liftIO getClockTime
       let newPage = pages !! ((fromIntegral picosecs `div` 1000000) `mod` length pages)
       seeOther (urlForPage newPage) $ toResponse $ p << "Redirecting to a random page"

showFrontPage :: String -> Params -> Web Response
showFrontPage _ params = do
  cfg <- getConfig
  showPage (frontPage cfg) params

showPage :: String -> Params -> Web Response
showPage page params = do
  jsMathExists <- queryAppState jsMath
  mbCached <- lookupCache (pathForPage page) (pRevision params)
  case mbCached of
         Just cp ->
           formattedPage (defaultPageLayout { pgScripts = ["jsMath/easy/load.js" | jsMathExists]}) page params $ cpContents cp
         _ -> do
           mDoc <- pageAsPandoc page params
           case mDoc of
                Just d  -> do
                  let divify c = thediv ! [identifier "wikipage",
                                            strAttr "onDblClick" ("window.location = '" ++ urlForPage page ++
                                            "?edit" ++
                                            (case (pRevision params) of
                                                  Nothing -> ""
                                                  Just r  -> urlEncodeVars [("revision", r),("logMsg", "Revert to " ++ r)]) ++ "';")] << c

                  c <- liftM divify $ pandocToHtml d
                  when (isNothing (pRevision params)) $ do
                    -- TODO not quite ideal, since page might have been modified after being retrieved by pageAsPandoc
                    -- better to have pageAsPandoc return the revision ID too...
                    fs <- getFileStore
                    rev <- liftIO $ latest fs (pathForPage page)
                    cacheContents (pathForPage page) rev c
                  formattedPage (defaultPageLayout { pgScripts = ["jsMath/easy/load.js" | jsMathExists]}) page params c
                Nothing -> createPage page params

discussPage :: String -> Params -> Web Response
discussPage page params = do
  if isDiscussPage page
     then showPage page params
     else showPage (page ++ ":discuss") params

createPage :: String -> Params -> Web Response
createPage page params =
  formattedPage (defaultPageLayout { pgTabs = [] }) page params $
     p << [ stringToHtml ("There is no page '" ++ page ++ "'.  You may create the page by ")
          , anchor ! [href $ urlForPage page ++ "?edit"] << "clicking here." ] 

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
  let upForm = form ! [X.method "post", enctype "multipart/form-data"] << fieldset <<
        [ p << [label << "File to upload:", br, afile "file" ! [value origPath] ]
        , p << [label << "Name on wiki, including extension",
                noscript << " (leave blank to use the same filename)", stringToHtml ":", br,
                textfield "wikiname" ! [value wikiname],
                primHtmlChar "nbsp", checkbox "overwrite" "yes", label << "Overwrite existing file"]
        , p << [label << "Description of content or changes:", br, textfield "logMsg" ! [size "60", value logMsg],
                submit "upload" "Upload"] ]
  formattedPage (defaultPageLayout { pgScripts = ["uploadForm.js"], pgShowPageTools = False, pgTabs = [], pgTitle = "Upload a file"} ) page params upForm

uploadFile :: String -> Params -> Web Response
uploadFile _ params = do
  let page = "_upload"
  let origPath = pFilename params
  let fileContents = pFileContents params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- getConfig
  mbUser <- getUser $ pUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  let overwrite = pOverwrite params
  fs <- getFileStore
  exists <- liftIO $ catch (latest fs wikiname >> return True) (\e -> if e == NotFound then return False else throwIO e >> return True)
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
       liftIO $ save fs wikiname (Author user email) logMsg fileContents
       formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Upload successful" }) page params $
                     thediv << [ h2 << ("Uploaded " ++ show (B.length fileContents) ++ " bytes")
                               , if takeExtension wikiname `elem` imageExtensions
                                    then p << "To add this image to a page, use:" +++
                                         pre << ("![alt text](/" ++ wikiname ++ ")")
                                    else p << "To link to this resource from a page, use:" +++
                                         pre << ("[link label](/" ++ wikiname ++ ")") ]
     else uploadForm page (params { pMessages = errors })

goToPage :: String -> Params -> Web Response
goToPage _ params = do
  let gotopage = pGotoPage params
  fs <- getFileStore
  allPageNames <- liftM (map dropExtension . filter (".page" `isSuffixOf`)) $ liftIO $ index fs
  let findPage f = find f allPageNames
  case findPage (gotopage ==) of
       Just m  -> seeOther (urlForPage m) $ toResponse "Redirecting to exact match"
       Nothing -> case findPage (\n -> (map toLower gotopage) == (map toLower n)) of
                       Just m  -> seeOther (urlForPage m) $ toResponse "Redirecting to case-insensitive match"
                       Nothing -> case findPage (\n -> (map toLower gotopage) `isPrefixOf` (map toLower n)) of
                                       Just m  -> seeOther (urlForPage m) $ toResponse "Redirecting to partial match"
                                       Nothing -> searchResults "" params{ pPatterns = words gotopage }

searchResults :: String -> Params -> Web Response
searchResults _ params = do
  let page = "_search"
  let patterns = pPatterns params
  let limit = pLimit params
  fs <- getFileStore
  matchLines <- if null patterns
                   then return []
                   else liftM (take limit) $ liftIO $ search fs defaultSearchQuery{queryPatterns = patterns}
  let contentMatches = map matchResourceName matchLines
  allPages <- liftM (filter (".page" `isSuffixOf`)) $ liftIO $ index fs
  let matchesPatterns pageName = all (`elem` (words $ map toLower $ dropExtension pageName)) $ map (map toLower) patterns
  let pageNameMatches = filter matchesPatterns allPages
  let allMatchedFiles = nub $ filter (".page" `isSuffixOf`) contentMatches ++ pageNameMatches
  let matches = map (\f -> (f, mapMaybe (\x -> if matchResourceName x == f then Just (matchLine x) else Nothing) matchLines)) allMatchedFiles
  let relevance (f, ms) = length ms + if f `elem` pageNameMatches then 100 else 0
  let preamble = if null matches
                    then h3 << if null patterns
                                  then ["Please enter a search term."]
                                  else ["No matches found for '", unwords patterns, "':"]
                    else h3 << [(show $ length matches), " matches found for '", unwords patterns, "':"]
  let htmlMatches = preamble +++ olist << map
                      (\(file, contents) -> li << [anchor ! [href $ urlForPage $ takeBaseName file] << takeBaseName file,
                      stringToHtml (" (" ++ show (length contents) ++ " matching lines)"),
                      stringToHtml " ", anchor ! [href "#", theclass "showmatch", thestyle "display: none;"] <<
                      if length contents > 0 then "[show matches]" else "",
                      pre ! [theclass "matches"] << unlines contents])
                      (reverse $ sortBy (comparing relevance) matches)
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgScripts = ["search.js"], pgTitle = "Search results"}) page params htmlMatches

preview :: String -> Params -> Web Response
preview _ params = pandocToHtml (textToPandoc $ pRaw params) >>= ok . setContentType "text/html" . toResponse . fromString . renderHtmlFragment

showPageHistory :: String -> Params -> Web Response
showPageHistory page params = showHistory (pathForPage page) page params

showFileHistory :: String -> Params -> Web Response
showFileHistory file params = showHistory file file params

showHistory :: String -> String -> Params -> Web Response
showHistory file page params =  do
  currTime <- liftIO getCurrentTime
  let oneYearAgo = addMinutes (-1 * 60 * 24 * 365) currTime
  let since = case pSince params of
                   Nothing -> Just oneYearAgo
                   Just t  -> Just t
  fs <- getFileStore
  hist <- liftIO $ history fs [file] (TimeRange since Nothing)
  if null hist
     then noHandle
     else do
       let versionToHtml rev pos = 
              li ! [theclass "difflink", intAttr "order" pos, strAttr "revision" $ revId rev] <<
                   [thespan ! [theclass "date"] << (show $ revDateTime rev), stringToHtml " (",
                    thespan ! [theclass "author"] <<
                            anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
                                       (authorName $ revAuthor rev), stringToHtml ")", stringToHtml ": ",
                    anchor ! [href (urlForPage page ++ "?revision=" ++ revId rev)] <<
                    thespan ! [theclass "subject"] <<  revDescription rev,
                    noscript << ([stringToHtml " [compare with ",
                    anchor ! [href $ urlForPage page ++ "?diff&from=" ++ revId rev ++
                              "^&to=" ++ revId rev] << "previous"] ++
                                 (if pos /= 1
                                     then [primHtmlChar "nbsp", primHtmlChar "bull",
                                           primHtmlChar "nbsp",
                                           anchor ! [href $ urlForPage page ++ "?diff&from=" ++
                                                     revId rev ++ "&to=HEAD"] << "current" ]
                                     else []) ++
                                 [stringToHtml "]"])]
       let contents = ulist ! [theclass "history"] << zipWith versionToHtml hist [(length hist), (length hist - 1)..1]
       formattedPage (defaultPageLayout { pgScripts = ["dragdiff.js"], pgSelectedTab = HistoryTab, pgTitle = ("Changes to " ++ page) }) page params contents

showActivity :: String -> Params -> Web Response
showActivity _ params = do
  let page = "_activity"
  currTime <- liftIO getCurrentTime
  let oneMonthAgo = addMinutes (-1 * 60 * 24 * 30) currTime
  let since = case pSince params of
                   Nothing -> Just oneMonthAgo
                   Just t  -> Just t
  let forUser = pForUser params
  fs <- getFileStore
  hist <- liftIO $ history fs [] (TimeRange since Nothing)
  let fileFromChange (Added f) = f
      fileFromChange (Modified f) = f
      fileFromChange (Deleted f) = f
  let filesFor changes revis = intersperse (primHtmlChar "nbsp") $ map
                             (\file -> anchor ! [href $ urlForPage file ++ "?diff&to=" ++ revis] << file) $ map
                             (\file -> if ".page" `isSuffixOf` file then dropExtension file else file) $ map fileFromChange changes 
  let heading = h1 << ("Recent changes" ++ if null forUser then "" else (" by " ++ forUser))
  let contents = ulist ! [theclass "history"] << map (\rev -> li <<
                           [thespan ! [theclass "date"] << (show $ revDateTime rev), stringToHtml " (",
                            thespan ! [theclass "author"] <<
                                    anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
                                               (authorName $ revAuthor rev), stringToHtml "): ",
                            thespan ! [theclass "subject"] << revDescription rev, stringToHtml " (",
                            thespan ! [theclass "files"] << filesFor (revChanges rev) (revId rev),
                            stringToHtml ")"]) hist
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Recent changes" }) page params (heading +++ contents)

showPageDiff :: String -> Params -> Web Response
showPageDiff page params = showDiff (pathForPage page) page params

showFileDiff :: String -> Params -> Web Response
showFileDiff page params = showDiff page page params

showDiff :: String -> String -> Params -> Web Response
showDiff file page params = do
  let from = pFrom params
  let to = pTo params
  fs <- getFileStore
  from' <- case from of
              Nothing -> do
                pageHist <- liftIO $ history fs [pathForPage page] (TimeRange Nothing Nothing)
                if length pageHist < 2
                   then return Nothing
                   else case to of
                            Nothing -> return Nothing
                            Just t  -> let (_, upto) = break (\r -> idsMatch fs (revId r) t) pageHist
                                       in  return $
                                           if length upto >= 2
                                              then Just $ revId $ upto !! 1  -- the immediately preceding revision
                                              else Nothing
              x       -> return x
  rawDiff <- liftIO $ diff fs file from' to
  let diffItemToHtml (B, xs) = thespan << xs
      diffItemToHtml (F, xs) = thespan ! [theclass "deleted"] << xs
      diffItemToHtml (S, xs) = thespan ! [theclass "added"]   << xs
  let formattedDiff = h2 ! [theclass "revision"] << ("Changes from " ++ case from' of { Just r -> r; Nothing -> "beginning" }) +++
                      pre ! [theclass "diff"] << map diffItemToHtml rawDiff
  formattedPage (defaultPageLayout { pgTabs = DiffTab : pgTabs defaultPageLayout, pgSelectedTab = DiffTab })
                page (params { pRevision = to }) formattedDiff

editPage :: String -> Params -> Web Response
editPage page params = do
  let rev = pRevision params
  let messages = pMessages params
  fs <- getFileStore
  (mbRev, raw) <- case pEditedText params of
                       Nothing -> liftIO $ catch
                                          (do c <- liftIO $ retrieve fs (pathForPage page) rev
                                              r <- liftIO $ case rev of
                                                                 Nothing  -> latest fs (pathForPage page) >>= revision fs
                                                                 Just r   -> revision fs r
                                              return $ (Just $ revId r, c))
                                          (\e -> if e == NotFound
                                                    then return (Nothing, "")
                                                    else throwIO e)
                       Just t -> return (if null (pSHA1 params) then Nothing else Just (pSHA1 params), t)
  let logMsg = pLogMsg params
  let sha1Box = case mbRev of
                 Just r  -> textfield "sha1" ! [thestyle "display: none", value r]
                 Nothing -> noHtml
  let editForm = gui (urlForPage page) ! [identifier "editform"] <<
                   [sha1Box,
                    textarea ! [cols "80", name "editedText", identifier "editedText"] << raw, br,
                    label << "Description of changes:", br,
                    textfield "logMsg" ! [value logMsg],
                    submit "update" "Save", primHtmlChar "nbsp",
                    submit "cancel" "Discard", primHtmlChar "nbsp",
                    input ! [thetype "button", theclass "editButton", identifier "previewButton",
                             strAttr "onClick" "updatePreviewPane();", strAttr "style" "display: none;", value "Preview" ],
                    thediv ! [ identifier "previewpane" ] << noHtml ]
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgSelectedTab = EditTab,
                                     pgScripts = ["preview.js"], pgTitle = ("Editing " ++ page) }) page (params {pMessages = messages}) editForm

confirmDelete :: String -> Params -> Web Response
confirmDelete page params = do
  let confirmForm = gui "" <<
        [ p << "Are you sure you want to delete this page?"
        , submit "confirm" "Yes, delete it!"
        , stringToHtml " "
        , submit "cancel" "No, keep it!"
        , br ]
  formattedPage defaultPageLayout page params confirmForm

deletePage :: String -> Params -> Web Response
deletePage page params = do
  mbUser <- getUser $ pUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  if pConfirm params
     then do
       fs <- getFileStore
       liftIO $ delete fs (pathForPage page) (Author user email) "Deleted using web interface."
       seeOther "/" $ toResponse $ p << "Page deleted"
     else seeOther (urlForPage page) $ toResponse $ p << "Page not deleted"

updatePage :: String -> Params -> Web Response
updatePage page params = do
  mbUser <- getUser $ pUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  let editedText = case pEditedText params of
                      Nothing -> error "No body text in POST request"
                      Just b  -> b
  let logMsg = pLogMsg params
  let oldSHA1 = pSHA1 params
  fs <- getFileStore
  if null logMsg
     then editPage page (params { pMessages = ["Description cannot be empty."] })
     else do
       cfg <- getConfig
       if length editedText > fromIntegral (maxUploadSize cfg)
          then error "Page exceeds maximum size."
          else return ()
       -- ensure that every file has a newline at the end, to avoid "No newline at eof" messages in diffs
       let editedText' = if null editedText || last editedText == '\n' then editedText else editedText ++ "\n"
       -- check SHA1 in case page has been modified, merge
       modifyRes <-    if null oldSHA1
                          then liftIO $ create fs (pathForPage page) (Author user email) logMsg editedText' >> return (Right ())
                          else liftIO $ catch (modify fs (pathForPage page) oldSHA1 (Author user email) logMsg editedText')
                                     (\e -> if e == Unchanged then return (Right ()) else throwIO e)
       case modifyRes of
            Right ()       -> seeOther (urlForPage page) $ toResponse $ p << "Page updated"
            Left (MergeInfo mergedWithRev False mergedText) ->
                              updatePage page params{ pMessages = ("Merged with revision " ++ revId mergedWithRev) : pMessages params,
                                                      pEditedText = Just mergedText,
                                                      pSHA1 = revId mergedWithRev }
            Left (MergeInfo mergedWithRev True mergedText) -> do
               let mergeMsg = "The page has been edited since you checked it out. " ++
                              "Changes have been merged into your edits below. " ++
                              "Please resolve conflicts and Save."
               editPage page (params { pEditedText = Just mergedText
                                     , pSHA1 = revId mergedWithRev
                                     , pMessages = [mergeMsg] })

indexPage :: String -> Params -> Web Response
indexPage _ params = do
  let page = "_index"
  fs <- getFileStore
  files <- liftIO $ index fs
  let htmlIndex = fileListToHtml "/" $ map splitPath $ sort $ filter (\f -> not (":discuss.page" `isSuffixOf` f)) files
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgScripts = ["folding.js"], pgTitle = "All pages" }) page params htmlIndex

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
  (map (\(h, l) -> let h' = if takeExtension h == ".page" then dropExtension h else h
                   in if [] `elem` l
                         then li ! [theclass $ if takeExtension h == ".page" then "page" else "upload"] << anchor ! [href $ prefix ++ h'] << h'
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
  cfg <- getConfig
  return $ writeHtml (defaultWriterOptions { writerStandalone = False
                                           , writerHTMLMathMethod = JsMath (Just "/js/jsMath/easy/load.js")
                                           , writerTableOfContents = tableOfContents cfg
                                           }) $ processPandoc convertWikiLinks pandocContents

-- | Abstract representation of page layout (tabs, scripts, etc.)
data PageLayout = PageLayout
  { pgTitle          :: String
  , pgScripts        :: [String]
  , pgShowPageTools  :: Bool
  , pgTabs           :: [Tab]
  , pgSelectedTab    :: Tab
  }

data Tab = ViewTab | EditTab | HistoryTab | DiscussTab | DiffTab deriving (Eq, Show)

defaultPageLayout :: PageLayout
defaultPageLayout = PageLayout
  { pgTitle          = ""
  , pgScripts        = []
  , pgShowPageTools  = True
  , pgTabs           = [ViewTab, EditTab, HistoryTab, DiscussTab]
  , pgSelectedTab    = ViewTab
  }

-- | Returns formatted page
formattedPage :: PageLayout -> String -> Params -> Html -> Web Response
formattedPage layout page params htmlContents = do
  let rev = pRevision params
  let path' = if isPage page then pathForPage page else page
  fs <- getFileStore
  sha1 <- case rev of
             Nothing -> liftIO $ catch (latest fs path')
                                       (\e -> if e == NotFound
                                                 then return ""
                                                 else throwIO e)
             Just r  -> return r
  user <- getLoggedInUser params
  let javascriptlinks = if null (pgScripts layout)
                           then ""
                           else renderHtmlFragment $ concatHtml $ map
                                  (\x -> script ! [src ("/js/" ++ x), thetype "text/javascript"] << noHtml)
                                  (["jquery.min.js", "jquery-ui.packed.js"] ++ pgScripts layout)
  let pageTitle = pgTitle layout `orIfNull` page
  let tabli tab = if tab == pgSelectedTab layout
                     then li ! [theclass "selected"]
                     else li
  let origPage s = if ":discuss" `isSuffixOf` s then take (length s - 8) s else s
  let linkForTab HistoryTab = Just $ tabli HistoryTab << anchor ! [href $ urlForPage page ++ "?history" ++ 
                                                                    case rev of { Just r -> "&revision" ++ r; Nothing -> "" }] << "history"
      linkForTab DiffTab    = Just $ tabli DiffTab << anchor ! [href ""] << "diff"
      linkForTab ViewTab    = if isDiscussPage page
                                 then Just $ tabli DiscussTab << anchor ! [href $ urlForPage $ origPage page] << "page"
                                 else Just $ tabli ViewTab << anchor ! [href $ urlForPage page ++
                                                                    case rev of { Just r -> "?revision=" ++ r; Nothing -> "" }] << "view"
      linkForTab DiscussTab = if isDiscussPage page
                                 then Just $ tabli ViewTab << anchor ! [href $ urlForPage page] << "discuss"
                                 else if isPage page
                                      then Just $ tabli DiscussTab << anchor ! [href $ urlForPage page ++ "?discuss"] << "discuss"
                                      else Nothing
      linkForTab EditTab    = if isPage page
                                 then Just $ tabli EditTab << anchor ! [href $ urlForPage page ++ "?edit" ++
                                              (case rev of
                                                    Just r   -> "&revision=" ++ r ++ "&" ++ urlEncodeVars [("logMsg", "Revert to " ++ r)]
                                                    Nothing  -> "")] <<
                                             if isNothing rev then "edit" else "revert"
                                 else Nothing
  let tabs = ulist ! [theclass "tabs"] << mapMaybe linkForTab (pgTabs layout)
  let searchbox = gui ("/_search") ! [identifier "searchform"] <<
                         [ textfield "patterns"
                         , submit "search" "Search" ]
  let gobox     = gui ("/_go") ! [identifier "goform"] <<
                         [ textfield "gotopage"
                         , submit "go" "Go" ]
  let messages = pMessages params
  let htmlMessages = if null messages
                        then noHtml
                        else ulist ! [theclass "messages"] << map (li <<) messages
  templ <- liftIO $ readIORef template
  let filledTemp = T.render $
                   T.setAttribute "pagetitle" pageTitle $
                   T.setAttribute "javascripts" javascriptlinks $
                   T.setAttribute "pagename" page $
                   (case user of
                         Just u     -> T.setAttribute "user" u
                         Nothing    -> id) $
                   (if isPage page then T.setAttribute "ispage" "true" else id) $
                   (if pgShowPageTools layout then T.setAttribute "pagetools" "true" else id) $
                   (if pPrintable params then T.setAttribute "printable" "true" else id) $
                   (if isJust rev then T.setAttribute "nothead" "true" else id) $
                   (if isJust rev then T.setAttribute "revision" (fromJust rev) else id) $
                   T.setAttribute "sha1" sha1 $
                   T.setAttribute "searchbox" (renderHtmlFragment (searchbox +++ gobox)) $
                   T.setAttribute "exportbox" (renderHtmlFragment $  exportBox page params) $
                   T.setAttribute "tabs" (renderHtmlFragment tabs) $
                   T.setAttribute "messages" (renderHtmlFragment htmlMessages) $
                   T.setAttribute "content" (renderHtmlFragment htmlContents) $
                   templ
  ok $ setContentType "text/html" $ toResponse $ fromString filledTemp

-- user authentication
loginForm :: Html
loginForm =
  gui "/_login" ! [identifier "loginForm"] << fieldset <<
     [ label << "Username ", textfield "username" ! [size "15"], stringToHtml " "
     , label << "Password ", X.password "password" ! [size "15"], stringToHtml " "
     , submit "login" "Login"] +++
     p << [ stringToHtml "If you do not have an account, "
          , anchor ! [href "/_register"] << "click here to get one." ]

loginUserForm :: String -> Params -> Web Response
loginUserForm page params =
  addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" $ fromMaybe "/" $ pReferer params) >>
  loginUserForm' page params

loginUserForm' :: String -> Params -> Web Response
loginUserForm' page params =
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Login" }) page params loginForm

loginUser :: String -> Params -> Web Response
loginUser page params = do
  let uname = pUsername params
  let pword = pPassword params
  let destination = pDestination params
  allowed <- authUser uname pword
  if allowed
    then do
      key <- newSession (SessionData uname)
      addCookie sessionTime (mkCookie "sid" (show key))
      addCookie 0 (mkCookie "destination" "")   -- remove unneeded destination cookie
      seeOther destination $ toResponse $ p << ("Welcome, " ++ uname)
    else
      loginUserForm' page (params { pMessages = "Authentication failed." : pMessages params })

logoutUser :: String -> Params -> Web Response
logoutUser _ params = do
  let key = pSessionKey params
  let destination = substitute " " "%20" $ fromMaybe "/" $ pReferer params
  case key of
       Just k  -> do
         delSession k
         addCookie 0 (mkCookie "sid" "")  -- make cookie expire immediately, effectively deleting it
       Nothing -> return ()
  seeOther destination $ toResponse "You have been logged out."

registerForm :: Web Html
registerForm = do
  cfg <- getConfig
  let accessQ = case accessQuestion cfg of
                      Nothing          -> noHtml
                      Just (prompt, _) -> label << prompt +++ br +++
                                          X.password "accessCode" ! [size "15"] +++ br
  let captcha = if useRecaptcha cfg
                   then captchaFields (recaptchaPublicKey cfg) Nothing
                   else noHtml
  return $ gui "" ! [identifier "loginForm"] << fieldset <<
            [ accessQ
            , label << "Username (at least 3 letters or digits):", br
            , textfield "username" ! [size "20"], stringToHtml " ", br
            , label << "Email (optional, will not be displayed on the Wiki):", br
            , textfield "email" ! [size "20"], br
            , textfield "fullname" ! [size "20", theclass "req"]
            , label << "Password (at least 6 characters, including at least one non-letter):", br
            , X.password "password" ! [size "20"], stringToHtml " ", br
            , label << "Confirm Password:", br, X.password "password2" ! [size "20"], stringToHtml " ", br
            , captcha
            , submit "register" "Register" ]

registerUserForm :: String -> Params -> Web Response
registerUserForm _ params =
  addCookie (60 * 10) (mkCookie "destination" $ substitute " " "%20" $ fromMaybe "/" $ pReferer params) >>
  registerForm >>=
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Register for an account" }) "_register" params

registerUser :: String -> Params -> Web Response
registerUser _ params = do
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
  let isValidAccessCode = case accessQuestion cfg of
        Nothing           -> True
        Just (_, answers) -> accessCode `elem` answers
  let isValidEmail e = length (filter (=='@') e) == 1
  captchaResult  <- if useRecaptcha cfg
                       then if null (recaptchaChallengeField recaptcha) || null (recaptchaResponseField recaptcha)
                               then return $ Left "missing-challenge-or-response"  -- no need to bother captcha.net in this case
                               else liftIO $ validateCaptcha (recaptchaPrivateKey cfg) (pIPAddress params) (recaptchaChallengeField recaptcha)
                                                              (recaptchaResponseField recaptcha)
                       else return $ Right ()
  let (validCaptcha, captchaError) = case captchaResult of
                                      Right () -> (True, Nothing)
                                      Left err -> (False, Just err)
  let errors = validate [ (taken, "Sorry, that username is already taken.")
                        , (not isValidAccessCode, "Incorrect response to access prompt.")
                        , (not (isValidUsername uname), "Username must be at least 3 charcaters, all letters or digits.")
                        , (not (isValidPassword pword), "Password must be at least 6 characters, with at least one non-letter.")
                        , (not (null email) && not (isValidEmail email), "Email address appears invalid.")
                        , (pword /= pword2, "Password does not match confirmation.")
                        , (not validCaptcha, "Failed CAPTCHA (" ++ fromJust captchaError ++ "). Are you really human?")
                        , (not (null fakeField), "You do not seem human enough.") ] -- fakeField is hidden in CSS (honeypot)
  if null errors
     then do
       user <- liftIO $ mkUser uname email pword
       addUser uname user
       loginUser "/" (params { pUsername = uname, pPassword = pword, pEmail = email })
     else registerForm >>=
          formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Register for an account" })
                    "_register" (params { pMessages = errors })

showHighlightedSource :: String -> Params -> Web Response
showHighlightedSource file params = do
  mbCached <- lookupCache file (pRevision params)
  case mbCached of
         Just cp -> formattedPage defaultPageLayout file params $ cpContents cp
         _ -> do
           contents <- rawContents file params
           case contents of
               Just source -> let lang' = head $ languagesByExtension $ takeExtension file
                              in case highlightAs lang' (filter (/='\r') source) of
                                       Left _       -> noHandle
                                       Right res    -> do
                                         let formattedContents = formatAsXHtml [OptNumberLines] lang' res
                                         when (isNothing (pRevision params)) $ do
                                           fs <- getFileStore
                                           rev <- liftIO $ latest fs file
                                           cacheContents file rev formattedContents
                                         formattedPage defaultPageLayout file params $ formattedContents
               Nothing     -> noHandle

defaultRespOptions :: WriterOptions
defaultRespOptions = defaultWriterOptions { writerStandalone = True, writerWrapText = True }

respondLaTeX :: String -> Pandoc -> Web Response
respondLaTeX page = ok . setContentType "application/x-latex" . setFilename (page ++ ".tex") . toResponse . fromString .
                    writeLaTeX (defaultRespOptions {writerHeader = defaultLaTeXHeader})

respondConTeXt :: String -> Pandoc -> Web Response
respondConTeXt page = ok . setContentType "application/x-context" . setFilename (page ++ ".tex") . toResponse . fromString .
                      writeConTeXt (defaultRespOptions {writerHeader = defaultConTeXtHeader})

respondRTF :: String -> Pandoc -> Web Response
respondRTF page = ok . setContentType "application/rtf" . setFilename (page ++ ".rtf") . toResponse . fromString .
                  writeRTF (defaultRespOptions {writerHeader = defaultRTFHeader})

respondRST :: String -> Pandoc -> Web Response
respondRST _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . fromString .
               writeRST (defaultRespOptions {writerHeader = "", writerReferenceLinks = True})

respondMan :: String -> Pandoc -> Web Response
respondMan _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . fromString .
               writeMan (defaultRespOptions {writerHeader = ""})

respondS5 :: String -> Pandoc -> Web Response
respondS5 _ = ok . toResponse . writeS5 (defaultRespOptions {writerHeader = defaultS5Header,
                                                             writerS5 = True, writerIncremental = True})

respondTexinfo :: String -> Pandoc -> Web Response
respondTexinfo page = ok . setContentType "application/x-texinfo" . setFilename (page ++ ".texi") . toResponse . fromString .
                      writeTexinfo (defaultRespOptions {writerHeader = ""})

respondDocbook :: String -> Pandoc -> Web Response
respondDocbook page = ok . setContentType "application/docbook+xml" . setFilename (page ++ ".xml") . toResponse . fromString .
                      writeDocbook (defaultRespOptions {writerHeader = defaultDocbookHeader})

respondMediaWiki :: String -> Pandoc -> Web Response
respondMediaWiki _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . fromString .
                     writeMediaWiki (defaultRespOptions {writerHeader = ""})

respondODT :: String -> Pandoc -> Web Response
respondODT page doc = do
  let openDoc = writeOpenDocument (defaultRespOptions {writerHeader = defaultOpenDocumentHeader}) doc
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                conf <- getConfig
                let repoPath = case repository conf of
                                Git path'   -> path'
                                Darcs path' -> path'
                saveOpenDocumentAsODT tempfile repoPath openDoc
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
exportBox page params | isPage page =
  let rev = pRevision params
  in  gui (urlForPage page) ! [identifier "exportbox"] << 
        ([ textfield "revision" ! [thestyle "display: none;", value (fromJust rev)] | isJust rev ] ++
         [ select ! [name "format"] <<
             map ((\f -> option ! [value f] << f) . fst) exportFormats
         , submit "export" "Export" ])
exportBox _ _ = noHtml

rawContents :: String -> Params -> Web (Maybe String)
rawContents file params = do
  let rev = pRevision params
  fs <- getFileStore
  liftIO $ catch (retrieve fs file rev >>= return . Just) (\e -> if e == NotFound then return Nothing else throwIO e)

removeRawHtmlBlock :: Block -> Block
removeRawHtmlBlock (RawHtml _) = RawHtml "<!-- raw HTML removed -->"
removeRawHtmlBlock x = x

textToPandoc :: String -> Pandoc
textToPandoc = processPandoc removeRawHtmlBlock .
               readMarkdown (defaultParserState { stateSanitizeHTML = True, stateSmart = True }) .
               filter (/= '\r')

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
  sysTempDir <- getTemporaryDirectory
  let dirName = sysTempDir </> baseName <.> show num
  liftIO $ catch (createDirectory dirName >> return dirName) $
      \e -> if isAlreadyExistsError e
               then createTempDir (num + 1) baseName
               else ioError e

