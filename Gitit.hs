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
import Data.Maybe (fromMaybe, fromJust, mapMaybe, isNothing, isJust)
import Data.Ord (comparing)
import qualified Data.Digest.SHA512 as SHA512 (hash)
import Paths_gitit
import Text.Pandoc
import Text.Pandoc.Definition (processPandoc)
import Text.Pandoc.Shared (HTMLMathMethod(..))
import Data.ByteString.Internal (c2w)
import Data.Char (isAlphaNum, isAlpha)
import Codec.Binary.UTF8.String (decodeString)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Network.HTTP (urlEncodeVars, urlEncode)
import System.Console.GetOpt
import System.Exit

gititVersion :: String
gititVersion = "0.1.1"

main :: IO ()
main = do
  argv <- getArgs
  options <- parseArgs argv
  conf <- foldM handleFlag defaultConfig options
  gitPath <- findExecutable "git"
  when (isNothing gitPath) $ error "'git' program not found in system path."
  initializeWiki (repositoryPath conf) (staticDir conf)
  control <- startSystemState entryPoint
  update $ SetConfig conf
  hPutStrLn stderr $ "Starting server on port " ++ show (portNumber conf)
  let debugger = if (debugMode conf) then debugFilter else id
  tid <- forkIO $ simpleHTTP (Conf { port = portNumber conf }) $ debugger $
          [ dir "stylesheets" [ fileServe [] $ (staticDir conf) </> "stylesheets" ]
          , dir "images"      [ fileServe [] $ (staticDir conf) </> "images" ]
          , dir "javascripts" [ fileServe [] $ (staticDir conf) </> "javascripts" ]
          ] ++ wikiHandlers ++ [ fileServe [] (repositoryPath conf) ]
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
initializeWiki :: FilePath -> FilePath -> IO ()
initializeWiki repodir staticdir = do
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
    B.writeFile "Front Page.page" welcomecontents
    B.writeFile "Help.page" helpcontents
    runCommand "git add 'Help.page' 'Front Page.page'; git commit -m 'Initial commit.'" >>= waitForProcess
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
                       "folding.js", "dragdiff.js", "preview.js", "search.js"]
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
wikiHandlers = [ dir "_index"    [ handle GET  indexPage ]
               , dir "_activity" [ handle GET  showActivity ]
               , dir "_preview"  [ handle POST preview ]
               , dir "_search"   [ handle POST searchResults ]
               , dir "_register" [ handle GET  registerUserForm,
                                   handle POST registerUser ]
               , dir "_login"    [ handle GET  loginUserForm,
                                   handle POST loginUser ]
               , dir "_logout"   [ handle GET  logoutUser ]
               , dir "_upload"   [ handle GET  uploadForm,
                                   handle POST uploadFile ]
               , handleCommand "showraw" GET  showRawPage
               , handleCommand "history" GET  showPageHistory
               , handleCommand "edit"    GET  (unlessNoEdit $ ifLoggedIn "edit" editPage)
               , handleCommand "diff"    GET  showDiff
               , handleCommand "cancel"  POST showPage
               , handleCommand "update"  POST (unlessNoEdit updatePage)
               , handleCommand "delete"  GET  (unlessNoDelete confirmDelete)
               , handleCommand "delete"  POST (unlessNoDelete deletePage)
               , handle GET showPage
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
                     , pSHA1         :: String
                     , pLogMsg       :: String
                     , pEmail        :: String
                     , pAccessCode   :: String
                     , pWikiname     :: String
                     , pOverwrite    :: Bool
                     , pFilename     :: String
                     , pFileContents :: B.ByteString
                     , pUser         :: String
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
         sh <- look "sha1"           `mplus` return ""
         lm <- look "logMsg"         `mplus` return ""
         em <- look "email"          `mplus` return ""
         wn <- look "wikiname"       `mplus` return ""
         ow <- (look "overwrite" >>= return . (== "yes")) `mplus` return False
         fn <- (lookInput "file" >>= return . fromMaybe "" . inputFilename) `mplus` return ""
         fc <- (lookInput "file" >>= return . inputValue) `mplus` return B.empty
         ac <- look "accessCode"     `mplus` return ""
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
                         , pSHA1         = sh
                         , pLogMsg       = lm
                         , pEmail        = em
                         , pWikiname     = wn
                         , pOverwrite    = ow
                         , pFilename     = fn
                         , pFileContents = fc
                         , pAccessCode   = ac
                         , pUser         = ""  -- this gets set by ifLoggedIn...
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
commandList = ["edit", "showraw", "history", "diff", "cancel", "update", "delete"]

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
ifLoggedIn command responder =
  \page params -> do user <- getLoggedInUser params
                     case user of
                          Nothing  -> seeOther ("/_login?" ++ urlEncodeVars [("destination", page ++ "?" ++ command)]) $ 
                                        toResponse $ p << "You must be logged in to edit a page."
                          Just u   -> responder page (params { pUser = u })

handle :: Method -> (String -> Params -> Web Response) -> Handler
handle meth responder = uriRest $ \uri -> let uriPath = drop 1 $ takeWhile (/='?') uri
                                          in  if isPage uriPath
                                                 then withData $ \params ->
                                                          [ withRequest $ \req -> if rqMethod req == meth
                                                                                     then responder uriPath params
                                                                                     else noHandle ]
                                                 else anyRequest noHandle

handleCommand :: String -> Method -> (String -> Params -> Web Response) -> Handler
handleCommand command meth responder =
  withData $ \com -> case com of
                          Command (Just c) | c == command -> [ handle meth responder ]
                          _                               -> []

orIfNull :: String -> String -> String
orIfNull str backup = if null str then backup else str

isPage :: String -> Bool
isPage ('_':_) = False
isPage s = '.' `notElem` s

urlForPage :: String -> String
urlForPage page = "/" ++ urlEncode page

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
showRawPage page params = do
  let revision = pRevision params
  rawContents <- gitCatFile revision (pathForPage page)
  case rawContents of
       Just c -> ok $ toResponse c
       _      -> noHandle

showPage :: String -> Params -> Web Response
showPage "" params = showPage "Front Page" params >>= seeOther "/Front%20Page"
showPage page params = do
  let revision = pRevision params
  rawContents <- gitCatFile revision (pathForPage page)
  case rawContents of
       Just c -> do
                 cont <- convertToHtml c
                 let cont' = thediv ! [identifier "wikipage",
                                       strAttr "onDblClick" ("window.location = '" ++ urlForPage page ++ "?edit&revision=" ++ revision ++
                                       (if revision == "HEAD" then "" else "&" ++ urlEncodeVars [("logMsg", "Revert to " ++ revision)]) ++ "';")] << cont
                 formattedPage [] ["jsMath/easy/load.js"] page params cont'
       _      -> if revision == "HEAD"
                    then editPage page params
                    else error $ "Invalid revision: " ++ revision

validate :: [(Bool, String)]   -- ^ list of conditions and error messages
         -> [String]           -- ^ list of error messages
validate = foldl go []
   where go errs (condition, msg) = if condition then msg:errs else errs

uploadForm :: String -> Params -> Web Response
uploadForm _ params = do
  let origPath = pFilename params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  let upForm = form ! [X.method "post", enctype "multipart/form-data"] <<
        [ p << [label << "File to upload:", br, afile "file" ! [value origPath]]
        , p << [label << "Name on wiki, including extension:", br, textfield "wikiname" ! [value wikiname],
                primHtmlChar "nbsp", checkbox "overwrite" "yes", label << "Overwrite existing file"]
        , p << [label << "Description of content or changes:", br, textfield "logMsg" ! [size "60", value logMsg],
                submit "upload" "Upload"] ]
  user <- getLoggedInUser params
  if isJust user
     then formattedPage [HidePageControls] [] "File upload" params upForm
     else seeOther ("/_login?" ++ urlEncodeVars [("destination", "_upload")]) $ toResponse $ p << "You must be logged in to upload a file."

uploadFile :: String -> Params -> Web Response
uploadFile _ params = do
  let origPath = pFilename params
  let fileContents = pFileContents params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  user <- getLoggedInUser params
  if isJust user
     then do
       cfg <- query GetConfig
       let author = fromJust user
       let email = ""
       let overwrite = pOverwrite params
       exists <- liftIO $ doesFileExist (repositoryPath cfg </> wikiname)
       let errors = validate [ (null logMsg, "Description cannot be empty.")
                             , (null origPath, "File not found.")
                             , (not overwrite && exists, "A file named '" ++ wikiname ++
                                "' already exists in the repository: choose a new name " ++
                                "or check the box to overwrite the existing file existing file.")
                             , (B.length fileContents > fromIntegral (maxUploadSize cfg),
                                "File exceeds maximum upload size.")
                             , (isPage wikiname,
                                "Uploaded file name must have an appropriate extension.")
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
             formattedPage [HidePageControls] [] "File upload" params $
                           p << ("Uploaded " ++ show (B.length fileContents) ++ " bytes")
           else uploadForm "File upload" (params { pMessages = errors })
     else seeOther ("/_login?" ++ urlEncodeVars [("destination", "_upload")]) $ toResponse $ p << "You must be logged in to upload a file."

searchResults :: String -> Params -> Web Response
searchResults _ params = do
  let patterns = pPatterns params
  let limit = pLimit params
  if null patterns
     then noHandle
     else do
       matchLines <- gitGrep patterns >>= return . map parseMatchLine . take limit . lines
       let matchedFiles = nub $ filter (".page" `isSuffixOf`) $ map fst matchLines
       let matches = map (\f -> (f, mapMaybe (\(a,b) -> if a == f then Just b else Nothing) matchLines)) matchedFiles
       let preamble = if null matches
                         then h3 << ["No matches found for '", unwords patterns, "':"]
                         else h3 << [(show $ length matches), " matches found for '", unwords patterns, "':"]
       let htmlMatches = preamble +++ olist << map
                           (\(file, contents) -> li << [anchor ! [href $ urlForPage $ takeBaseName file] << takeBaseName file,
                           stringToHtml (" (" ++ show (length contents) ++ " matching lines)"),
                           stringToHtml " ", anchor ! [href "#", theclass "showmatch", thestyle "display: none;"] << "[show matches]",
                           pre ! [theclass "matches"] << unlines contents])
                           (reverse  $ sortBy (comparing (length . snd)) matches)
       formattedPage [HidePageControls] ["search.js"] "Search Results" params htmlMatches

-- Auxiliary function for searchResults
parseMatchLine :: String -> (String, String)
parseMatchLine matchLine =
  let (file, rest) = break (==':') matchLine
      contents = drop 1 rest -- strip off colon
  in  (file, contents)

preview :: String -> Params -> Web Response
preview _ params = convertToHtml (pRaw params) >>= ok . toResponse

showPageHistory :: String -> Params -> Web Response
showPageHistory page params =  do
  let since = pSince params `orIfNull` "1 year ago"
  hist <- gitLog since "" [pathForPage page]
  let versionToHtml entry pos = li ! [theclass "difflink", intAttr "order" pos, strAttr "revision" $ logRevision entry] <<
                                      [thespan ! [theclass "date"] << logDate entry, stringToHtml " (",
                                       thespan ! [theclass "author"] << anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", logAuthor entry)]] <<
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
  let since = pSince params `orIfNull` "1 month ago"
  let forUser = pForUser params
  hist <- gitLog since forUser []
  let filesFor files  = intersperse (primHtmlChar "nbsp") $ map
                           (\file -> anchor ! [href $ urlForPage file ++ "?history"] << file) $ map
                           (\file -> if ".page" `isSuffixOf` file then dropExtension file else file) files
  let contents = ulist ! [theclass "history"] << map (\entry -> li <<
                           [thespan ! [theclass "date"] << logDate entry, stringToHtml " (",
                            thespan ! [theclass "author"] << anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", logAuthor entry)]] <<
                                                         (logAuthor entry), stringToHtml "):",
                            thespan ! [theclass "subject"] << logSubject entry, stringToHtml " (",
                            thespan ! [theclass "files"] << filesFor (logFiles entry),
                            stringToHtml ")"]) hist
  formattedPage [HidePageControls] [] ("Recent changes" ++ if null forUser then "" else (" by " ++ forUser)) params contents

showDiff :: String -> Params -> Web Response
showDiff page params = do
  let from = pFrom params
  let to = pTo params
  rawDiff <- gitDiff (pathForPage page) from to
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
  rawContents <- case pEditedText params of
                      Nothing -> gitCatFile revision (pathForPage page)
                      Just t  -> return $ Just t
  let (new, contents) = case rawContents of
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
  user <- getLoggedInUser params
  if isJust user
     then formattedPage [HidePageControls] [] page params confirmForm
     else seeOther ("/_login?" ++ urlEncodeVars [("destination", page ++ "?delete")]) $ toResponse $ p << "You must be logged in to delete a page."

deletePage :: String -> Params -> Web Response
deletePage page params = do
  user <- getLoggedInUser params
  if isNothing user
     then fail "User must be logged in to delete page."
     else return ()
  let author = fromJust user
  let email = ""
  gitRemove (pathForPage page) (author, email) "Deleted from web."
  seeOther "/" $ toResponse $ p << "Page deleted"

updatePage :: String -> Params -> Web Response
updatePage page params = do
  user <- getLoggedInUser params
  if isNothing user
     then fail "User must be logged in to update page."
     else return ()
  let editedText = case pEditedText params of
                      Nothing -> error "No body text in POST request"
                      Just b  -> b
  let author = fromJust user
  let email = ""
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
       -- check SHA1 in case page has been modified, merge
       if currentSHA1 == oldSHA1
          then do
            let dir' = takeDirectory page
            liftIO $ createDirectoryIfMissing True ((repositoryPath cfg) </> dir')
            liftIO $ writeFile ((repositoryPath cfg) </> pathForPage page) editedText
            gitCommit (pathForPage page) (author, email) logMsg
            seeOther (urlForPage page) $ toResponse $ p << "Page updated"
          else do -- there have been conflicting changes
            original <- gitCatFile oldSHA1 (pathForPage page) >>= return . fromJust
            latest <- gitCatFile currentSHA1 (pathForPage page) >>= return . fromJust
            let pagePath = repositoryPath cfg </> pathForPage page
            let [textTmp, originalTmp, latestTmp] = map (pagePath ++) [".edited",".original",".latest"]
            let editedText' = if null editedText || last editedText == '\n' then editedText else editedText ++ "\n"
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
  let revision = pRevision params
  files <- gitLsTree revision >>= return . map (unwords . drop 3 . words) . lines
  let htmlIndex = fileListToHtml "/" $ map splitPath $ sort files
  formattedPage [HidePageControls] ["folding.js"] "index" params htmlIndex

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

-- | Converts markdown string to HTML.
convertToHtml :: MonadIO m => String -> m Html
convertToHtml text' = do
  cfg <- query GetConfig
  let pandocContents = readMarkdown (defaultParserState { stateSanitizeHTML = True, stateSmart = True }) $
                                    filter (/= '\r') $ decodeString $ text'
  let htmlContents   = writeHtml (defaultWriterOptions { writerStandalone = False
                                                       , writerHTMLMathMethod = JsMath (Just "/javascripts/jsMath/easy/load.js")
                                                       , writerTableOfContents = tableOfContents cfg
                                                       }) $ processPandoc convertWikiLinks pandocContents
  return htmlContents

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
  let title' = thetitle << (wikiTitle cfg ++ " - " ++ page)
  let head' = header << [title', stylesheetlinks, javascriptlinks]
  let sitenav = thediv ! [theclass "sitenav"] <<
                        gui ("/_search") ! [identifier "searchform"] <<
                        [ anchor ! [href "/Front%20Page", theclass "nav_link"] << "front"
                        , primHtmlChar "bull"
                        , anchor ! [href "/_index", theclass "nav_link"] << "index"
                        , primHtmlChar "bull"
                        , anchor ! [href "/_upload", theclass "nav_link"] << "upload"
                        , primHtmlChar "bull"
                        , anchor ! [href "/_activity", theclass "nav_link"] << "activity"
                        , primHtmlChar "bull"
                        , anchor ! [href "/Help", theclass "nav_link"] << "help"
                        , primHtmlChar "nbsp"
                        , textfield "patterns" ! [theclass "search_field search_term"]
                        , submit "search" "Search" ]
  let buttons =    [ anchor ! [href $ urlForPage page ++ "?revision=" ++ revision ++ "&showraw", theclass "nav_link"] << "raw"
                   , primHtmlChar "bull"
                   , anchor ! [href $ urlForPage page ++ "?delete", theclass "nav_link"] << "delete"
                   , primHtmlChar "bull"
                   , anchor ! [href $ urlForPage page ++ "?revision=" ++ revision ++ "&history", theclass "nav_link"] << "history"
                   , primHtmlChar "bull"
                   , anchor ! [href $ urlForPage page ++ "?edit&revision=" ++ revision ++
                                      if revision == "HEAD" then "" else "&" ++ urlEncodeVars [("logMsg", "Revert to " ++ revision)],
                               theclass "nav_link"] << if revision == "HEAD" then "edit" else "revert" ]
  let userbox =    thediv ! [identifier "userbox"] <<
                        case user of
                             Just u   -> anchor ! [href ("/_logout?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] << ("logout " ++ u)
                             Nothing  -> (anchor ! [href ("/_login?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] << "login") +++
                                         primHtmlChar "bull" +++
                                         anchor ! [href ("/_register?" ++ urlEncodeVars [("destination", page)]), theclass "nav_link"] << "register"
  let sitenavVis = if HideNavbar `elem` opts then "hidden" else "visible"
  let sidebarVis = if HidePageControls `elem` opts then "hidden" else "visible"
  let messages = pMessages params
  let htmlMessages = if null messages
                        then noHtml
                        else ulist ! [theclass "messages"] << map (li <<) messages
  let body' = body << thediv ! [identifier "container"] <<
                        [ thediv ! [identifier "banner"] << primHtml (wikiBanner cfg)
                        , thediv ! [identifier "navbar", thestyle $ "visibility: " ++ sitenavVis] << [userbox, sitenav]
                        , thediv ! [identifier "pageTitle"] << [ anchor ! [href $ urlForPage page] << (h1 << page) ]
                        , thediv ! [identifier "content"] << [htmlMessages, htmlContents]
                        , thediv ! [identifier "pageinfo"] << [ thediv ! [theclass "pageControls", thestyle $ "visibility: " ++ sidebarVis] << buttons
                                                              , thediv ! [theclass "details"] << revision ]
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
loginUserForm _ params = formattedPage [HidePageControls] [] "Login" params $ loginForm params

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
      seeOther ("/" ++ intercalate "%20" (words destination)) $ toResponse $ p << ("Welcome, " ++ uname)
    else
      loginUserForm "Login" (params { pMessages = "Authentication failed." : pMessages params })

logoutUser :: String -> Params -> Web Response
logoutUser _ params = do
  let key = pSessionKey params
  let destination = pDestination params
  case key of
       Just k  -> update $ DelSession k
       Nothing -> return ()
  seeOther ("/" ++ intercalate "%20" (words destination)) $ toResponse $ p << "You have been logged out."

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
              textfield "username" ! [size "15"], stringToHtml " ", br,
              textfield "email" ! [size "15", theclass "req"],
              label << "Password (at least 6 characters, including at least one non-letter):", br,
              X.password "password" ! [size "15"], stringToHtml " ", br,
              label << "Confirm Password:", br, X.password "password2" ! [size "15"], stringToHtml " ", br,
              submit "register" "Register" ]

registerUserForm :: String -> Params -> Web Response
registerUserForm _ params = do
  regForm <- registerForm
  formattedPage [HidePageControls] [] "Register" params regForm

registerUser :: String -> Params -> Web Response
registerUser _ params = do
  regForm <- registerForm
  let isValidUsername u = length u >= 3 && all isAlphaNum u
  let isValidPassword pw = length pw >= 6 && not (all isAlpha pw)
  let accessCode = pAccessCode params
  let uname = pUsername params
  let pword = pPassword params
  let pword2 = pPassword2 params
  let fakeField = pEmail params
  taken <- query $ IsUser uname
  cfg <- query GetConfig
  let isValidAccessCode = case accessQuestion cfg of
        Nothing           -> True
        Just (_, answers) -> accessCode `elem` answers
  let errors = validate [ (taken, "Sorry, that username is already taken.")
                        , (not isValidAccessCode, "Incorrect response to access prompt.")
                        , (not (isValidUsername uname), "Username must be at least 3 charcaters, all letters or digits.")
                        , (not (isValidPassword pword), "Password must be at least 6 characters, with at least one non-letter.")
                        , (pword /= pword2, "Password does not match confirmation.")
                        , (not (null fakeField), "You do not seem human enough.") ] -- fakeField is hidden in CSS (honeypot)
  if null errors
     then do
       let passwordHash = SHA512.hash $ map c2w $ passwordSalt cfg ++ pword
       update $ AddUser uname (User { username = uname, password = passwordHash })
       loginUser "Front Page" (params { pUsername = uname, pPassword = pword, pDestination = "Front Page" })
     else formattedPage [HidePageControls] [] "Register" (params { pMessages = errors }) regForm

