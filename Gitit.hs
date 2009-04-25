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

import Gitit.Plugins ( loadPlugin )
import Gitit.Server
import Gitit.Util (orIfNull)
import Gitit.Initialize (createStaticIfMissing, createRepoIfMissing)
import Gitit.Framework
import Gitit.Layout
import Gitit.ContentTransformer (showRawPage, showFileAsText, showPage, exportPage, showHighlightedSource, preview)
import System.IO.UTF8
import System.IO (stderr)
import Control.Exception (throwIO, catch, try)
import Prelude hiding (writeFile, readFile, catch)
import System.Directory
import System.Time
import Control.Concurrent
import System.FilePath
import Gitit.State
import Gitit.Config (getConfigFromOpts)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( password, method )
import Data.List (intersperse, nub, sortBy, isSuffixOf, find, isPrefixOf, inits)
import Data.Maybe (fromMaybe, fromJust, mapMaybe, isNothing, isJust)
import qualified Data.Map as M
import Data.Ord (comparing)
import Paths_gitit
import Text.Pandoc.Shared (substitute)
import Data.Char (isAlphaNum, isAlpha, toLower)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Network.HTTP (urlEncodeVars)
import qualified Text.StringTemplate as T
import Data.DateTime (getCurrentTime, addMinutes)
import Network.Captcha.ReCaptcha (captchaFields, validateCaptcha)
import Data.FileStore
import System.Log.Logger (logM, Priority(..), setLevel, setHandlers, getLogger, saveGlobalLogger)
import System.Log.Handler.Simple (fileHandler)


main :: IO ()
main = do

  -- parse options to get config file
  conf' <- getConfigFromOpts
  let conf = if debugMode conf' then conf'{logLevel = DEBUG} else conf'

  -- check for external programs that are needed
  let prereqs = "grep" : case repository conf of
                      Git _        -> ["git"]
                      Darcs _      -> ["darcs"]
  forM_ prereqs $ \prog ->
    findExecutable prog >>= \mbFind ->
    when (isNothing mbFind) $ error $
      "Required program '" ++ prog ++ "' not found in system path."

  -- read user file and update state
  userFileExists <- doesFileExist $ userFile conf
  users' <- if userFileExists
               then liftM (M.fromList . read) $ readFile $ userFile conf
               else return M.empty

  -- set up logging
  let level = logLevel conf
  logFileHandler <- fileHandler (logFile conf) level
  serverLogger <- getLogger "Happstack.Server" -- changes to "Happstack.Server.AccessLog.Combined" for 0.3
  gititLogger <- getLogger "gitit"
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] serverLogger
  saveGlobalLogger $ setLevel level $ setHandlers [logFileHandler] gititLogger

  -- log config file in DEBUG
  logM "gitit" DEBUG (show conf)

  -- create template file if it doesn't exist
  let templatefile = templateFile conf
  templateExists <- doesFileExist templatefile
  unless templateExists $ do
    templatePath <- getDataFileName $ "data" </> "template.html"
    copyFile templatePath templatefile
    hPutStrLn stderr $ "Created " ++ templatefile

  -- read template file
  templ <- liftM T.newSTMP $ liftIO $ readFile templatefile

  -- initialize state
  initializeAppState conf users' templ

  -- load plugins
  let loadPluginAndLog plg = logM "gitit" WARNING ("Loading plugin '" ++ plg ++ "'...") >> loadPlugin plg
  plugins' <- mapM loadPluginAndLog (pluginModules conf)
  updateAppState $ \st -> plugins' `seq` st { plugins = plugins' }
  unless (null $ pluginModules conf) $ logM "gitit" WARNING "Finished loading plugins."

  -- setup the page repository and static files, if they don't exist
  createRepoIfMissing conf
  let staticdir = staticDir conf
  createStaticIfMissing staticdir

  -- start the server
  tid <- forkIO $ simpleHTTP (Conf { validator = Nothing, port = portNumber conf }) $ msum $
          map (\d -> dir d (withExpiresHeaders $ fileServe [] $ staticdir </> d)) ["css", "img", "js"] ++
          [ debugHandler | debugMode conf ] ++
          wikiHandlers
  waitForTermination

  -- shut down the server
  killThread tid

wikiHandlers :: [Handler]
wikiHandlers = [ handle isIndex          GET indexPage
               , handle isPreview        POST preview
               , handlePath "_activity"  GET  showActivity
               , handlePath "_go"        POST goToPage
               , handlePath "_search"    POST searchResults
               , handlePath "_search"    GET  searchResults
               , handlePath "_register"  GET  registerUserForm
               , handlePath "_register"  POST registerUser
               , handlePath "_login"     GET  loginUserForm
               , handlePath "_login"     POST loginUser
               , handlePath "_logout"    GET  logoutUser
               , handlePath "_upload"    GET  (ifLoggedIn uploadForm loginUserForm)
               , handlePath "_upload"    POST (ifLoggedIn uploadFile loginUserForm)
               , handlePath "_random"    GET  randomPage
               , handlePath ""           GET  showFrontPage
               , withCommand "showraw" [ handlePage GET showRawPage
                                       , handle isSourceCode GET showFileAsText ]
               , withCommand "history" [ handlePage GET showPageHistory
                                       , handle isSourceCode GET showFileHistory ]
               , withCommand "edit"    [ handlePage GET $ unlessNoEdit (ifLoggedIn editPage loginUserForm) showPage ]
               , withCommand "diff"    [ handlePage GET showPageDiff
                                       , handle isSourceCode GET showFileDiff ]
               , withCommand "export"  [ handlePage POST exportPage
                                       , handlePage GET exportPage ]
               , withCommand "cancel"  [ handlePage POST showPage ]
               , withCommand "discuss" [ handlePage GET discussPage ]
               , withCommand "update"  [ handlePage POST $ unlessNoEdit (ifLoggedIn updatePage loginUserForm) showPage ]
               , withCommand "delete"  [ handlePage GET  $ unlessNoDelete (ifLoggedIn confirmDelete loginUserForm) showPage,
                                         handlePage POST $ unlessNoDelete (ifLoggedIn deletePage loginUserForm) showPage ]
               , handlePage GET showPage
               , handle isSourceCode GET showHighlightedSource
               , handleAny
               , handlePage GET  createPage
               , handlePage POST createPage  -- this will happen if they click Discard on a new page
               ]

isIndex :: String -> Bool
isIndex "_index" = True
isIndex x        = "_index/" `isPrefixOf` x

isPreview :: String -> Bool
isPreview x  = "___preview" `isSuffixOf` x
-- We choose something that is unlikely to occur naturally as a suffix.
-- Why suffix and not prefix?  Because the link is added by a script,
-- and mod_proxy_html doesn't rewrite links in scripts.  So this is
-- to make it possible to use gitit with an alterantive docroot.

handleAny :: Handler
handleAny = 
  uriRest $ \uri -> let path' = uriPath uri
                    in  do fs <- getFileStore
                           mimetype <- getMimeTypeForExtension (takeExtension path')
                           res <- liftIO $ try $ (retrieve fs path' Nothing  :: IO B.ByteString)
                           case res of
                                  Right contents -> anyRequest $ ok $ setContentType mimetype $
                                                               (toResponse noHtml) {rsBody = contents} -- ugly hack
                                  Left NotFound  -> anyRequest mzero
                                  Left e         -> error (show e)

debugHandler :: Handler
debugHandler = do
  withRequest $ \req -> liftIO $ logM "gitit" DEBUG (show req)
  msum [ handle (const True) GET showParams, handle (const True) POST showParams ]
    where showParams page params = do
            liftIO $ logM "gitit" DEBUG $ "Page = '" ++ page ++ "'\n" ++ show params
            mzero

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

discussPage :: String -> Params -> Web Response
discussPage page _ = seeOther (urlForPage discussionPage) $ toResponse "Redirecting to discussion page"
    where discussionPage = if isDiscussPage page then page else page ++ ":discuss"

createPage :: String -> Params -> Web Response
createPage page params =
  formattedPage (defaultPageLayout { pgTabs = [] }) page params $
     p << [ stringToHtml ("There is no page '" ++ page ++ "'.  You may create the page by ")
          , anchor ! [href $ urlForPage page ++ "?edit"] << "clicking here." ] 

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
                        , (takeExtension wikiname == ".page",
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

showPageHistory :: String -> Params -> Web Response
showPageHistory page params = showHistory (pathForPage page) page params

showFileHistory :: String -> Params -> Web Response
showFileHistory file = showHistory file file

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
     then mzero
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
                    anchor ! [href $ urlForPage page ++ "?diff&to=" ++ revId rev] << "previous"] ++
                                 (if pos /= 1
                                     then [primHtmlChar "nbsp", primHtmlChar "bull",
                                           primHtmlChar "nbsp",
                                           anchor ! [href $ urlForPage page ++ "?diff&from=" ++
                                                     revId rev] << "current" ]
                                     else []) ++
                                 [stringToHtml "]"])]
       let contents = ulist ! [theclass "history"] << zipWith versionToHtml hist [(length hist), (length hist - 1)..1]
       let tabs = if file == page  -- source file, not wiki page
                     then [ViewTab,HistoryTab]
                     else pgTabs defaultPageLayout
       formattedPage (defaultPageLayout { pgScripts = ["dragdiff.js"], pgTabs = tabs, pgSelectedTab = HistoryTab, pgTitle = ("Changes to " ++ page) }) page params contents

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
  let hist' = case forUser of
                   Nothing -> hist
                   Just u  -> filter (\r -> authorName (revAuthor r) == u) hist
  let fileFromChange (Added f) = f
      fileFromChange (Modified f) = f
      fileFromChange (Deleted f) = f
  let filesFor changes revis = intersperse (primHtmlChar "nbsp") $ map
                             (\file -> anchor ! [href $ urlForPage file ++ "?diff&to=" ++ revis] << file) $ map
                             (\file -> if ".page" `isSuffixOf` file then dropExtension file else file) $ map fileFromChange changes 
  let heading = h1 << ("Recent changes by " ++ fromMaybe "all users" forUser)
  let contents = ulist ! [theclass "history"] << map (\rev -> li <<
                           [thespan ! [theclass "date"] << (show $ revDateTime rev), stringToHtml " (",
                            thespan ! [theclass "author"] <<
                                    anchor ! [href $ "/_activity?" ++ urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
                                               (authorName $ revAuthor rev), stringToHtml "): ",
                            thespan ! [theclass "subject"] << revDescription rev, stringToHtml " (",
                            thespan ! [theclass "files"] << filesFor (revChanges rev) (revId rev),
                            stringToHtml ")"]) hist'
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Recent changes" }) page params (heading +++ contents)

showPageDiff :: String -> Params -> Web Response
showPageDiff page = showDiff (pathForPage page) page

showFileDiff :: String -> Params -> Web Response
showFileDiff page = showDiff page page

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
  let diffLineToHtml (B, xs) = thespan << unlines xs
      diffLineToHtml (F, xs) = thespan ! [theclass "deleted"] << unlines xs
      diffLineToHtml (S, xs) = thespan ! [theclass "added"]   << unlines xs
  let formattedDiff = h2 ! [theclass "revision"] << ("Changes from " ++ case from' of { Just r -> r; Nothing -> "beginning" }) +++
                      pre ! [theclass "diff"] << map diffLineToHtml rawDiff
  formattedPage (defaultPageLayout { pgTabs = DiffTab : pgTabs defaultPageLayout, pgSelectedTab = DiffTab })
                page (params { pRevision = to }) formattedDiff

editPage :: String -> Params -> Web Response
editPage page params = do
  let rev = pRevision params  -- if this is set, we're doing a revert
  fs <- getFileStore
  let getRevisionAndText = catch 
                           (do c <- liftIO $ retrieve fs (pathForPage page) rev
                               -- even if pRevision is set, we return revId of latest
                               -- saved version (because we're doing a revert and
                               -- we don't want gitit to merge the changes with the
                               -- latest version)
                               r <- liftIO $ latest fs (pathForPage page) >>= revision fs
                               return (Just $ revId r, c))
                           (\e -> if e == NotFound
                                     then return (Nothing, "")
                                     else throwIO e)
  (mbRev, raw) <- case pEditedText params of
                       Nothing -> liftIO getRevisionAndText 
                       Just t  -> let r = if null (pSHA1 params)
                                             then Nothing
                                             else Just (pSHA1 params)
                                  in return (r, t)
  let messages = pMessages params
  let logMsg = pLogMsg params
  let sha1Box = case mbRev of
                 Just r  -> textfield "sha1" ! [thestyle "display: none", value r]
                 Nothing -> noHtml
  let readonly = if isJust (pRevision params) -- disable editing of text box if it's a revert
                    then [strAttr "readonly" "yes", strAttr "style" "color: gray"]
                    else [] 
  let editForm = gui (urlForPage page) ! [identifier "editform"] <<
                   [sha1Box,
                    textarea ! (readonly ++ [cols "80", name "editedText", identifier "editedText"]) <<
                    raw, br,
                    label << "Description of changes:", br,
                    textfield "logMsg" ! (readonly ++ [value logMsg]),
                    submit "update" "Save", primHtmlChar "nbsp",
                    submit "cancel" "Discard", primHtmlChar "nbsp",
                    input ! [thetype "button", theclass "editButton", identifier "previewButton",
                             strAttr "onClick" "updatePreviewPane();",
                             strAttr "style" "display: none;", value "Preview" ],
                    thediv ! [ identifier "previewpane" ] << noHtml ]
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgSelectedTab = EditTab,
                                     pgScripts = ["preview.js"], pgTitle = ("Editing " ++ page) })
                page (params {pMessages = messages}) editForm

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
       -- check SHA1 in case page has been modified, merge
       modifyRes <-    if null oldSHA1
                          then liftIO $ create fs (pathForPage page) (Author user email) logMsg editedText >> return (Right ())
                          else liftIO $ catch (modify fs (pathForPage page) oldSHA1 (Author user email) logMsg editedText)
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
indexPage page params = do
  fs <- getFileStore
  let prefix'  = dropWhile (=='/') . drop 6 $ page   -- drop the "_index/" part
  let prefix'' = if null prefix' then "" else prefix' ++ "/"
  listing <- liftIO $ directory fs prefix'
  let isDiscussionPage (FSFile f) = ":discuss.page" `isSuffixOf` f
      isDiscussionPage (FSDirectory _) = False
  let prunedListing = filter (not . isDiscussionPage) listing
  let htmlIndex = fileListToHtml prefix'' prunedListing
  formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgScripts = [], pgTitle = "Contents"}) page params htmlIndex

fileListToHtml :: String -> [Resource] -> Html

fileListToHtml prefix files =
  let fileLink (FSFile f) | takeExtension f == ".page" =
                 li ! [theclass "page"  ] << anchor ! [href $ "/" ++ prefix ++ dropExtension f] << dropExtension f
      fileLink (FSFile f) = li ! [theclass "upload"] << anchor ! [href $ "/" ++ prefix ++ f] << f
      fileLink (FSDirectory f) = li ! [theclass "folder"] << anchor ! [href $ "/_index/" ++ prefix ++ f] << f
      uplink =  let updirs = drop 1 $ inits $ splitPath $ "/" ++ prefix
                in  foldr (\d accum ->  concatHtml [ anchor ! [theclass "updir", href $ "/_index" ++ joinPath d] <<
                                           last d, accum]) noHtml updirs
  in uplink +++ ulist ! [theclass "index"] << map fileLink files

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
      addCookie 0 (mkCookie "destination" "/")   -- remove unneeded destination cookie
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
         addCookie 0 (mkCookie "sid" "-1")  -- make cookie expire immediately, effectively deleting it
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
            , textfield "full_name_1" ! [size "20", theclass "req"]
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
                               else liftIO $ do
                                      mbIPaddr <- lookupIPAddr $ pPeer params
                                      let ipaddr = case mbIPaddr of
                                                        Just ip -> ip
                                                        Nothing -> error $ "Could not find ip address for " ++ pPeer params
                                      ipaddr `seq` validateCaptcha (recaptchaPrivateKey cfg) ipaddr (recaptchaChallengeField recaptcha)
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
                        , (not (null fakeField), "You do not seem human enough. If you're sure you are human, " ++
                                                 "try turning off form auto-completion in your browser.") ] -- fakeField is hidden in CSS (honeypot)
  if null errors
     then do
       user <- liftIO $ mkUser uname email pword
       addUser uname user
       loginUser "/" (params { pUsername = uname, pPassword = pword, pEmail = email })
     else registerForm >>=
          formattedPage (defaultPageLayout { pgShowPageTools = False, pgTabs = [], pgTitle = "Register for an account" })
                    "_register" (params { pMessages = errors })

