{-
Copyright (C) 2008-9 John MacFarlane <jgm@berkeley.edu>

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

{- Handlers for wiki functions.
-}

module Gitit.Handlers ( handleAny
                      , debugHandler
                      , randomPage
                      , discussPage
                      , createPage
                      , showActivity
                      , goToPage
                      , searchResults
                      , uploadForm
                      , uploadFile
                      , indexPage
                      , preview
                      , showRawPage
                      , showFileAsText
                      , showPageHistory
                      , showFileHistory
                      , showPage
                      , showPageDiff
                      , showFileDiff
                      , exportPage
                      , updatePage
                      , editPage
                      , deletePage
                      , confirmDelete
                      , showHighlightedSource
                      , loginUserForm
                      , loginUser
                      , registerUserForm
                      , registerUser
                      , resetPasswordRequestForm
                      , resetPasswordRequest
                      , resetPassword
                      , doResetPassword
                      , authHandler
                      )
where
import Data.FileStore
import Gitit.Server
import Gitit.Framework
import Gitit.Layout
import Gitit.State
import Gitit.Types
import Gitit.Util (orIfNull)
import Gitit.Authentication
import Gitit.ContentTransformer (showRawPage, showFileAsText, showPage,
        exportPage, showHighlightedSource, preview, applyPreCommitPlugins)
import Control.Exception (throwIO, catch, try)
import Prelude hiding (writeFile, readFile, catch)
import System.Time
import System.FilePath
import Gitit.State
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( method )
import Data.List (intersperse, nub, sortBy, find, isPrefixOf, inits)
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Ord (comparing)
import Data.Char (toLower)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import Network.HTTP (urlEncodeVars)
import Data.DateTime (getCurrentTime, addMinutes)
import Data.FileStore
import System.Log.Logger (logM, Priority(..))

handleAny :: Handler
handleAny = uriRest $ \uri ->
  let path' = uriPath uri
  in  do fs <- getFileStore
         mimetype <- getMimeTypeForExtension
                      (takeExtension path')
         res <- liftIO $ try
                (retrieve fs path' Nothing :: IO B.ByteString)
         case res of
                Right contents -> anyRequest $ ok $ setContentType mimetype $
                                    (toResponse noHtml) {rsBody = contents}
                                    -- ugly hack
                Left NotFound  -> anyRequest mzero
                Left e         -> error (show e)

debugHandler :: Handler
debugHandler = do
  withRequest $ \req -> liftIO $ logM "gitit" DEBUG (show req)
  msum [ handle (const True) GET showParams,
         handle (const True) POST showParams ]
    where showParams page params = do
            liftIO $ logM "gitit" DEBUG $ "Page = '" ++ page ++ "'\n" ++
              show params
            mzero

randomPage :: String -> Params -> Web Response
randomPage _ _ = do
  fs <- getFileStore
  files <- liftIO $ index fs
  let pages = map dropExtension $
                filter (\f -> isPageFile f && not (isDiscussPageFile f)) files
  if null pages
     then error "No pages found!"
     else do
       TOD _ picosecs <- liftIO getClockTime
       let newPage = pages !!
                     ((fromIntegral picosecs `div` 1000000) `mod` length pages)
       seeOther (urlForPage newPage) $ toResponse $
         p << "Redirecting to a random page"

discussPage :: String -> Params -> Web Response
discussPage page _ = seeOther (urlForPage discussionPage) $
                     toResponse "Redirecting to discussion page"
    where discussionPage = if isDiscussPage page
                              then page
                              else page ++ ":discuss"

createPage :: String -> Params -> Web Response
createPage page params =
  formattedPage (defaultPageLayout { pgTabs = [] }) page params $
     p << [ stringToHtml ("There is no page '" ++ page ++
               "'.  You may create the page by "),
             anchor ! [href $ urlForPage page ++ "?edit"] << "clicking here." ]

uploadForm :: String -> Params -> Web Response
uploadForm _ params = do
  let page = "_upload"
  let origPath = pFilename params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  let upForm = form ! [X.method "post", enctype "multipart/form-data"] <<
       fieldset <<
       [ p << [label << "File to upload:"
              , br
              , afile "file" ! [value origPath] ]
       , p << [ label << "Name on wiki, including extension"
              , noscript << " (leave blank to use the same filename)"
              , stringToHtml ":"
              , br
              , textfield "wikiname" ! [value wikiname]
              , primHtmlChar "nbsp"
              , checkbox "overwrite" "yes"
              , label << "Overwrite existing file" ]
       , p << [ label << "Description of content or changes:"
              , br
              , textfield "logMsg" ! [size "60", value logMsg]
              , submit "upload" "Upload" ]
       ]
  formattedPage defaultPageLayout{
                   pgScripts = ["uploadForm.js"],
                   pgShowPageTools = False,
                   pgTabs = [],
                   pgTitle = "Upload a file"}
                page params upForm

uploadFile :: String -> Params -> Web Response
uploadFile _ params = do
  let page = "_upload"
  let origPath = pFilename params
  let fileContents = pFileContents params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- getConfig
  mbUser <- getLoggedInUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  let overwrite = pOverwrite params
  fs <- getFileStore
  exists <- liftIO $ catch (latest fs wikiname >> return True) $ \e ->
                      if e == NotFound
                         then return False
                         else throwIO e >> return True
  let imageExtensions = [".png", ".jpg", ".gif"]
  let errors = validate
                 [ (null logMsg, "Description cannot be empty.")
                 , (null origPath, "File not found.")
                 , (not overwrite && exists, "A file named '" ++ wikiname ++
                    "' already exists in the repository: choose a new name " ++
                    "or check the box to overwrite the existing file.")
                 , (B.length fileContents > fromIntegral (maxUploadSize cfg),
                    "File exceeds maximum upload size.")
                 , (isPageFile wikiname,
                    "This file extension is reserved for wiki pages.")
                 ]
  if null errors
     then do
       liftIO $ save fs wikiname (Author user email) logMsg fileContents
       let contents = thediv <<
             [ h2 << ("Uploaded " ++ show (B.length fileContents) ++ " bytes")
             , if takeExtension wikiname `elem` imageExtensions
                  then p << "To add this image to a page, use:" +++
                       pre << ("![alt text](/" ++ wikiname ++ ")")
                  else p << "To link to this resource from a page, use:" +++
                       pre << ("[link label](/" ++ wikiname ++ ")") ]
       formattedPage defaultPageLayout{
                       pgShowPageTools = False,
                       pgTabs = [],
                       pgTitle = "Upload successful"}
                     page params contents
     else uploadForm page params{ pMessages = errors }

goToPage :: String -> Params -> Web Response
goToPage _ params = do
  let gotopage = pGotoPage params
  fs <- getFileStore
  allPageNames <- liftM (map dropExtension . filter isPageFile) $
                   liftIO $ index fs
  let findPage f = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = (map toLower gotopage) == (map toLower f)
  let prefixMatch f = (map toLower gotopage) `isPrefixOf` (map toLower f)
  case findPage exactMatch of
       Just m  -> seeOther (urlForPage m) $ toResponse
                     "Redirecting to exact match"
       Nothing -> case findPage insensitiveMatch of
                       Just m  -> seeOther (urlForPage m) $ toResponse
                                    "Redirecting to case-insensitive match"
                       Nothing -> case findPage prefixMatch of
                                       Just m  -> seeOther (urlForPage m) $
                                                  toResponse $ "Redirecting" ++
                                                    " to partial match"
                                       Nothing -> searchResults "" params{
                                                    pPatterns = words gotopage}

searchResults :: String -> Params -> Web Response
searchResults _ params = do
  let page = "_search"
  let patterns = pPatterns params
  let limit = pLimit params
  fs <- getFileStore
  matchLines <- if null patterns
                   then return []
                   else liftM (take limit) $ liftIO $
                          search fs defaultSearchQuery{queryPatterns = patterns}
  let contentMatches = map matchResourceName matchLines
  allPages <- liftM (filter isPageFile) $ liftIO $ index fs
  let inPageName pageName x = x `elem` (words $ map toLower $
                                         dropExtension pageName)
  let matchesPatterns pageName = all (inPageName pageName) $
                                   map (map toLower) patterns
  let pageNameMatches = filter matchesPatterns allPages
  let allMatchedFiles = nub $ filter isPageFile contentMatches ++
                              pageNameMatches
  let matchesInFile f =  mapMaybe (\x -> if matchResourceName x == f
                                            then Just (matchLine x)
                                            else Nothing) matchLines
  let matches = map (\f -> (f, matchesInFile f)) allMatchedFiles
  let relevance (f, ms) = length ms + if f `elem` pageNameMatches
                                         then 100
                                         else 0
  let preamble = if null matches
                    then h3 << if null patterns
                                  then ["Please enter a search term."]
                                  else ["No matches found for '",
                                         unwords patterns, "':"]
                    else h3 << [(show $ length matches),
                                " matches found for '", unwords patterns, "':"]
  let toMatchListItem (file, contents) = li <<
        [ anchor ! [href $ urlForPage $ takeBaseName file] << takeBaseName file
        , stringToHtml (" (" ++ show (length contents) ++ " matching lines)")
        , stringToHtml " "
        , anchor ! [href "#", theclass "showmatch",
                    thestyle "display: none;"] << if length contents > 0
                                                     then "[show matches]"
                                                     else ""
        , pre ! [theclass "matches"] << unlines contents]
  let htmlMatches = preamble +++
                    olist << map toMatchListItem
                             (reverse $ sortBy (comparing relevance) matches)
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = "Search results"}
                page params htmlMatches

showPageHistory :: String -> Params -> Web Response
showPageHistory page = showHistory (pathForPage page) page

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
  let versionToHtml rev pos = li ! [theclass "difflink", intAttr "order" pos,
                                    strAttr "revision" $ revId rev] <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] << anchor ! [href $ "/_activity?" ++
            urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
              (authorName $ revAuthor rev)
        , stringToHtml "): "
        , anchor ! [href (urlForPage page ++ "?revision=" ++ revId rev)] <<
           thespan ! [theclass "subject"] <<  revDescription rev
        , noscript <<
            ([ stringToHtml " [compare with "
             , anchor ! [href $ urlForPage page ++ "?diff&to=" ++ revId rev] <<
                  "previous" ] ++
             (if pos /= 1
                  then [ primHtmlChar "nbsp"
                       , primHtmlChar "bull"
                       , primHtmlChar "nbsp"
                       , anchor ! [href $ urlForPage page ++ "?diff&from=" ++
                                  revId rev] << "current"
                       ]
                  else []) ++
             [stringToHtml "]"])
        ]
  if null hist
     then mzero
     else do
       let contents = ulist ! [theclass "history"] <<
                        zipWith versionToHtml hist
                        [(length hist), (length hist - 1)..1]
       let tabs = if file == page  -- source file, not wiki page
                     then [ViewTab,HistoryTab]
                     else pgTabs defaultPageLayout
       formattedPage defaultPageLayout{
                        pgScripts = ["dragdiff.js"],
                        pgTabs = tabs,
                        pgSelectedTab = HistoryTab,
                        pgTitle = ("Changes to " ++ page)
                        }
                     page params contents

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
  let fileFromChange (Added f)    = f
      fileFromChange (Modified f) = f
      fileFromChange (Deleted f)  = f
  let dropDotPage file = if isPageFile file
                            then dropExtension file
                            else file
  let fileAnchor revis file =
        anchor ! [href $ urlForPage file ++ "?diff&to=" ++ revis] << file
  let filesFor changes revis = intersperse (primHtmlChar "nbsp") $
        map (fileAnchor revis . dropDotPage . fileFromChange) changes
  let heading = h1 << ("Recent changes by " ++ fromMaybe "all users" forUser)
  let revToListItem rev = li <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] <<
            anchor ! [href $ "/_activity?" ++
              urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
                (authorName $ revAuthor rev)
        , stringToHtml "): "
        , thespan ! [theclass "subject"] << revDescription rev
        , stringToHtml " ("
        , thespan ! [theclass "files"] << filesFor (revChanges rev) (revId rev)
        , stringToHtml ")"
        ]
  let contents = ulist ! [theclass "history"] << map revToListItem hist'
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Recent changes"
                  }
                page params (heading +++ contents)

showPageDiff :: String -> Params -> Web Response
showPageDiff page = showDiff (pathForPage page) page

showFileDiff :: String -> Params -> Web Response
showFileDiff page = showDiff page page

showDiff :: String -> String -> Params -> Web Response
showDiff file page params = do
  let from = pFrom params
  let to = pTo params
  fs <- getFileStore
  result <- liftIO $ try $ getDiff fs file from to
  case result of
       Left NotFound  -> mzero
       Left e         -> liftIO $ throwIO e
       Right htmlDiff -> formattedPage defaultPageLayout{
                                          pgTabs = DiffTab :
                                                   pgTabs defaultPageLayout,
                                          pgSelectedTab = DiffTab
                                          }
                                       page params{ pRevision = to } htmlDiff

getDiff :: FileStore -> FilePath -> Maybe RevisionId -> Maybe RevisionId
        -> IO Html
getDiff fs file from to = do
  from' <- case from of
              Just x  -> return $ Just x
              Nothing -> do
                pageHist <- liftIO $ history fs [file]
                                     (TimeRange Nothing Nothing)
                if length pageHist < 2
                   then return Nothing
                   else case to of
                            Nothing -> return Nothing
                            Just t  -> let (_, upto) = break
                                             (\r -> idsMatch fs (revId r) t)
                                             pageHist
                                       in  return $
                                           if length upto >= 2
                                              -- immediately preceding revision
                                              then Just $ revId $ upto !! 1
                                              else Nothing
  rawDiff <- diff fs file from' to
  let diffLineToHtml (B, xs) = thespan << unlines xs
      diffLineToHtml (F, xs) = thespan ! [theclass "deleted"] << unlines xs
      diffLineToHtml (S, xs) = thespan ! [theclass "added"]   << unlines xs
  return $ h2 ! [theclass "revision"] <<
             ("Changes from " ++
               case from' of
                    Just r  -> r
                    Nothing -> "beginning") +++
           pre ! [theclass "diff"] << map diffLineToHtml rawDiff

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
                 Just r  -> textfield "sha1" ! [thestyle "display: none",
                                                value r]
                 Nothing -> noHtml
  let readonly = if isJust (pRevision params)
                    -- disable editing of text box if it's a revert
                    then [strAttr "readonly" "yes",
                          strAttr "style" "color: gray"]
                    else []
  let editForm = gui (urlForPage page) ! [identifier "editform"] <<
                   [ sha1Box
                   , textarea ! (readonly ++ [cols "80", name "editedText",
                                  identifier "editedText"]) << raw
                   , br
                   , label << "Description of changes:"
                   , br
                   , textfield "logMsg" ! (readonly ++ [value logMsg])
                   , submit "update" "Save"
                   , primHtmlChar "nbsp"
                   , submit "cancel" "Discard"
                   , primHtmlChar "nbsp"
                   , input ! [thetype "button", theclass "editButton",
                              identifier "previewButton",
                              strAttr "onClick" "updatePreviewPane();",
                              strAttr "style" "display: none;",
                              value "Preview" ]
                   , thediv ! [ identifier "previewpane" ] << noHtml
                   ]
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgShowSiteNav = False,
                  pgShowMarkupHelp = True,
                  pgSelectedTab = EditTab,
                  pgScripts = ["preview.js"],
                  pgTitle = ("Editing " ++ page)
                  }
                page params{pMessages = messages} editForm

confirmDelete :: String -> Params -> Web Response
confirmDelete page params = do
  fs <- getFileStore
  -- determine whether there is a corresponding page, and if not whether there
  -- is a corresponding file
  pageTest <- liftIO $ try $ latest fs (pathForPage page)
  fileToDelete <- case pageTest of
                       Right _        -> return $ pathForPage page  -- a page
                       Left  NotFound -> do
                         fileTest <- liftIO $ try $ latest fs page
                         case fileTest of
                              Right _       -> return page  -- a source file
                              Left NotFound -> return ""
                              Left e        -> fail (show e)
                       Left e        -> fail (show e)
  let confirmForm = gui "" <<
        [ p << "Are you sure you want to delete this page?"
        , input ! [thetype "text", name "filetodelete",
                   strAttr "style" "display: none;", value fileToDelete]
        , submit "confirm" "Yes, delete it!"
        , stringToHtml " "
        , submit "cancel" "No, keep it!"
        , br ]
  formattedPage defaultPageLayout page params $
    if null fileToDelete
       then ulist ! [theclass "messages"] << li <<
            "There is no file or page by that name."
       else confirmForm

deletePage :: String -> Params -> Web Response
deletePage page params = do
  mbUser <- getLoggedInUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  let author = Author user email
  let descrip = "Deleted using web interface."
  if pConfirm params
     then do
       fs <- getFileStore
       liftIO $ delete fs (pFileToDelete params) author descrip
       seeOther "/" $ toResponse $ p << "File deleted"
     else seeOther (urlForPage page) $ toResponse $ p << "Page not deleted"

updatePage :: String -> Params -> Web Response
updatePage page params = do
  mbUser <- getLoggedInUser params
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  editedText <- case pEditedText params of
                     Nothing -> error "No body text in POST request"
                     Just b  -> applyPreCommitPlugins page params b
  let logMsg = pLogMsg params
  let oldSHA1 = pSHA1 params
  fs <- getFileStore
  if null logMsg
     then editPage page params{ pMessages = ["Description cannot be empty."] }
     else do
       cfg <- getConfig
       when (length editedText > fromIntegral (maxUploadSize cfg)) $
          error "Page exceeds maximum size."
       -- check SHA1 in case page has been modified, merge
       modifyRes <- if null oldSHA1
                       then liftIO $ create fs (pathForPage page)
                                       (Author user email) logMsg editedText >>
                                     return (Right ())
                       else liftIO $ catch (modify fs (pathForPage page)
                                            oldSHA1 (Author user email) logMsg
                                            editedText)
                                     (\e -> if e == Unchanged
                                               then return (Right ())
                                               else throwIO e)
       case modifyRes of
            Right () -> seeOther (urlForPage page) $ toResponse $
               p << "Page updated"
            Left (MergeInfo mergedWithRev False mergedText) ->
               updatePage page params{
                 pMessages = ("Merged with revision " ++ revId mergedWithRev) :
                   pMessages params,
                 pEditedText = Just mergedText,
                 pSHA1 = revId mergedWithRev }
            Left (MergeInfo mergedWithRev True mergedText) -> do
               let mergeMsg =
                     "The page has been edited since you checked it out. " ++
                     "Changes have been merged into your edits below. " ++
                     "Please resolve conflicts and Save."
               editPage page params{
                                pEditedText = Just mergedText
                                , pSHA1 = revId mergedWithRev
                                , pMessages = [mergeMsg]
                                }

indexPage :: String -> Params -> Web Response
indexPage page params = do
  let prefix' = if "_index" `isPrefixOf` page
                   then dropWhile (=='/') $ drop 6 page
                   else page
  fs <- getFileStore
  listing <- liftIO $ directory fs prefix'
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = False
  let prunedListing = filter (not . isDiscussionPage) listing
  let htmlIndex = fileListToHtml prefix' prunedListing
  formattedPage defaultPageLayout{
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = [],
                  pgTitle = "Contents"}
                page params htmlIndex

fileListToHtml :: String -> [Resource] -> Html
fileListToHtml prefix files =
  let fileLink (FSFile f) | isPageFile f =
        li ! [theclass "page"  ] <<
          anchor ! [href $ "/" ++ prefix ++ dropExtension f] << dropExtension f
      fileLink (FSFile f) =
        li ! [theclass "upload"] << anchor ! [href $ "/" ++ prefix ++ f] << f
      fileLink (FSDirectory f) =
        li ! [theclass "folder"] <<
          anchor ! [href $ "/" ++ prefix ++ f ++ "/"] << f
      updirs = drop 1 $ inits $ splitPath $ '/' : prefix
      uplink = foldr (\d accum ->
                  concatHtml [ anchor ! [theclass "updir",
                                         href $ if length d == 1
                                                   then "/_index"
                                                   else joinPath d] <<
                  last d, accum]) noHtml updirs
  in uplink +++ ulist ! [theclass "index"] << map fileLink files

authHandler :: Handler
authHandler = msum $
  [ handlePath "_register"  GET  registerUserForm
  , handlePath "_register"  POST registerUser
  , handlePath "_login"     GET  loginUserForm
  , handlePath "_login"     POST loginUser
  , handlePath "_logout"    GET  logoutUser
  , handlePath "_resetPassword"   GET  resetPasswordRequestForm
  , handlePath "_resetPassword"   POST resetPasswordRequest
  , handlePath "_doResetPassword" GET  resetPassword
  , handlePath "_doResetPassword" POST doResetPassword
  ]


