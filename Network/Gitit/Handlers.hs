{-# LANGUAGE ScopedTypeVariables #-}
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

module Network.Gitit.Handlers (
                        handleAny
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
                      , categoryPage
                      , categoryListPage
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
                      , currentUser
                      , expireCache
                      , feedHandler
                      )
where
import Safe
import Data.FileStore.Utils (isInsideDir)
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Types
import Network.Gitit.Feed (filestoreToXmlFeed, FeedConfig(..))
import Network.Gitit.Util (orIfNull, readFileUTF8)
import Network.Gitit.Cache (expireCachedFile, lookupCache, cacheContents)
import Network.Gitit.ContentTransformer (showRawPage, showFileAsText, showPage,
        exportPage, showHighlightedSource, preview, applyPreCommitPlugins)
import Network.Gitit.Page (extractCategories)
import Control.Exception (throwIO, catch, try)
import Data.ByteString.UTF8 (toString)
import System.Time
import System.FilePath
import Prelude hiding (catch)
import Network.Gitit.State
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import qualified Text.XHtml as X ( method )
import Data.List (intersperse, nub, sortBy, find, isPrefixOf, inits, sort)
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)
import Data.Ord (comparing)
import Data.Char (toLower, isSpace)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
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
                Right contents -> ignoreFilters >>  -- don't compress
                                  (ok $ setContentType mimetype $
                                    (toResponse noHtml) {rsBody = contents})
                                    -- ugly hack
                Left NotFound  -> anyRequest mzero
                Left e         -> error (show e)

debugHandler :: Handler
debugHandler = withData $ \(params :: Params) -> do
  req <- askRq
  liftIO $ logM "gitit" DEBUG (show req)
  page <- getPage
  liftIO $ logM "gitit" DEBUG $ "Page = '" ++ page ++ "'\n" ++
              show params
  mzero

randomPage :: Handler
randomPage = do
  fs <- getFileStore
  files <- liftIO $ index fs
  let pages = map dropExtension $
                filter (\f -> isPageFile f && not (isDiscussPageFile f)) files
  base' <- getWikiBase
  if null pages
     then error "No pages found!"
     else do
       TOD _ picosecs <- liftIO getClockTime
       let newPage = pages !!
                     ((fromIntegral picosecs `div` 1000000) `mod` length pages)
       seeOther (base' ++ urlForPage newPage) $ toResponse $
         p << "Redirecting to a random page"

discussPage :: Handler
discussPage = do
  page <- getPage
  base' <- getWikiBase
  seeOther (base' ++ urlForPage (if isDiscussPage page then page else ('@':page))) $
                     toResponse "Redirecting to discussion page"

createPage :: Handler
createPage = do
  page <- getPage
  base' <- getWikiBase
  case page of
       ('_':_) -> mzero   -- don't allow creation of _index, etc.
       _       -> formattedPage defaultPageLayout{
                                      pgPageName = page
                                    , pgTabs = []
                                    , pgTitle = "Create " ++ page ++ "?"
                                    } $
                    p << [ stringToHtml ("There is no page '" ++ page ++
                              "'.  You may create the page by "),
                            anchor ! [href $ base' ++ "/_edit" ++ urlForPage page] <<
                              "clicking here." ]

uploadForm :: Handler
uploadForm = withData $ \(params :: Params) -> do
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
                   pgMessages = pMessages params,
                   pgScripts = ["uploadForm.js"],
                   pgShowPageTools = False,
                   pgTabs = [],
                   pgTitle = "Upload a file"} upForm

uploadFile :: Handler
uploadFile = withData $ \(params :: Params) -> do
  let origPath = pFilename params
  let fileContents = pFileContents params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- getConfig
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  let overwrite = pOverwrite params
  fs <- getFileStore
  exists <- liftIO $ catch (latest fs wikiname >> return True) $ \e ->
                      if e == NotFound
                         then return False
                         else throwIO e >> return True
  inStaticDir <- liftIO $
                  (repositoryPath cfg </> wikiname) `isInsideDir` staticDir cfg
  inTemplatesDir <- liftIO $
                  (repositoryPath cfg </> wikiname) `isInsideDir` templatesDir cfg
  let imageExtensions = [".png", ".jpg", ".gif"]
  let errors = validate
                 [ (null . filter (not . isSpace) $ logMsg,
                    "Description cannot be empty.")
                 , (null origPath, "File not found.")
                 , (inStaticDir,  "Destination is inside static directory.")
                 , (inTemplatesDir,  "Destination is inside templates directory.")
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
       expireCachedFile wikiname `mplus` return ()
       liftIO $ save fs wikiname (Author user email) logMsg fileContents
       let contents = thediv <<
             [ h2 << ("Uploaded " ++ show (B.length fileContents) ++ " bytes")
             , if takeExtension wikiname `elem` imageExtensions
                  then p << "To add this image to a page, use:" +++
                       pre << ("![alt text](/" ++ wikiname ++ ")")
                  else p << "To link to this resource from a page, use:" +++
                       pre << ("[link label](/" ++ wikiname ++ ")") ]
       formattedPage defaultPageLayout{
                       pgMessages = pMessages params,
                       pgShowPageTools = False,
                       pgTabs = [],
                       pgTitle = "Upload successful"}
                     contents
     else withMessages errors uploadForm

goToPage :: Handler
goToPage = withData $ \(params :: Params) -> do
  let gotopage = pGotoPage params
  fs <- getFileStore
  allPageNames <- liftM (map dropExtension . filter isPageFile) $
                   liftIO $ index fs
  let findPage f = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = (map toLower gotopage) == (map toLower f)
  let prefixMatch f = (map toLower gotopage) `isPrefixOf` (map toLower f)
  base' <- getWikiBase
  case findPage exactMatch of
       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                     "Redirecting to exact match"
       Nothing -> case findPage insensitiveMatch of
                       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                                    "Redirecting to case-insensitive match"
                       Nothing -> case findPage prefixMatch of
                                       Just m  -> seeOther (base' ++ urlForPage m) $
                                                  toResponse $ "Redirecting" ++
                                                    " to partial match"
                                       Nothing -> withInput "patterns" gotopage searchResults

searchResults :: Handler
searchResults = withData $ \(params :: Params) -> do
  let patterns = pPatterns params
  fs <- getFileStore
  matchLines <- if null patterns
                   then return []
                   else liftIO $ catch (search fs SearchQuery{
                                                  queryPatterns = patterns
                                                , queryWholeWords = True
                                                , queryMatchAll = True
                                                , queryIgnoreCase = True })
                                       -- catch error, because newer versions of git
                                       -- return 1 on no match, and filestore <=0.3.3
                                       -- doesn't handle this properly:
                                       (\(_ :: FileStoreError)  -> return [])
  let contentMatches = map matchResourceName matchLines
  allPages <- liftM (filter isPageFile) $ liftIO $ index fs
  let slashToSpace = map (\c -> if c == '/' then ' ' else c)
  let inPageName pageName' x = x `elem` (words $ slashToSpace $ dropExtension pageName')
  let matchesPatterns pageName' = not (null patterns) &&
       all (inPageName (map toLower pageName')) (map (map toLower) patterns)
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
  let preamble = if null patterns 
                    then h3 << ["Please enter a search term."]
                    else h3 << [ stringToHtml (show (length matches) ++ " matches found for ")
                               , thespan ! [identifier "pattern"] << unwords patterns]
  base' <- getWikiBase
  let toMatchListItem (file, contents) = li <<
        [ anchor ! [href $ base' ++ urlForPage (dropExtension file)] << dropExtension file
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
                  pgMessages = pMessages params,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = "Search results"}
                htmlMatches

showPageHistory :: Handler
showPageHistory = withData $ \(params :: Params) -> do
  page <- getPage
  showHistory (pathForPage page) page params

showFileHistory :: Handler
showFileHistory = withData $ \(params :: Params) -> do
  file <- getPage
  showHistory file file params

showHistory :: String -> String -> Params -> Handler
showHistory file page params =  do
  currTime <- liftIO getCurrentTime
  let oneYearAgo = addMinutes (-1 * 60 * 24 * 365) currTime
  let since = case pSince params of
                   Nothing -> Just oneYearAgo
                   Just t  -> Just t
  fs <- getFileStore
  hist <- liftIO $ history fs [file] (TimeRange since Nothing)
  base' <- getWikiBase
  let versionToHtml rev pos = li ! [theclass "difflink", intAttr "order" pos,
                                    strAttr "revision" (revId rev),
                                    strAttr "diffurl" (base' ++ "/_diff/" ++ page)] <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] << anchor ! [href $ base' ++ "/_activity?" ++
            urlEncodeVars [("forUser", authorName $ revAuthor rev)]] <<
              (authorName $ revAuthor rev)
        , stringToHtml "): "
        , anchor ! [href (base' ++ urlForPage page ++ "?revision=" ++ revId rev)] <<
           thespan ! [theclass "subject"] <<  revDescription rev
        , noscript <<
            ([ stringToHtml " [compare with "
             , anchor ! [href $ base' ++ "/_diff" ++ urlForPage page ++ "?to=" ++ revId rev] <<
                  "previous" ] ++
             (if pos /= 1
                  then [ primHtmlChar "nbsp"
                       , primHtmlChar "bull"
                       , primHtmlChar "nbsp"
                       , anchor ! [href $ base' ++ "/_diff" ++ urlForPage page ++ "?from=" ++
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
                        pgPageName = page,
                        pgMessages = pMessages params,
                        pgScripts = ["dragdiff.js"],
                        pgTabs = tabs,
                        pgSelectedTab = HistoryTab,
                        pgTitle = ("Changes to " ++ page)
                        } contents

showActivity :: Handler
showActivity = withData $ \(params :: Params) -> do
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
  base' <- getWikiBase
  let fileAnchor revis file =
        anchor ! [href $ base' ++ "/_diff" ++ urlForPage file ++ "?to=" ++ revis] << file
  let filesFor changes revis = intersperse (primHtmlChar "nbsp") $
        map (fileAnchor revis . dropDotPage . fileFromChange) changes
  let heading = h1 << ("Recent changes by " ++ fromMaybe "all users" forUser)
  let revToListItem rev = li <<
        [ thespan ! [theclass "date"] << (show $ revDateTime rev)
        , stringToHtml " ("
        , thespan ! [theclass "author"] <<
            anchor ! [href $ base' ++ "/_activity?" ++
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
                  pgMessages = pMessages params,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Recent changes"
                  } (heading +++ contents)

showPageDiff :: Handler
showPageDiff = withData $ \(params :: Params) -> do
  page <- getPage
  showDiff (pathForPage page) page params

showFileDiff :: Handler
showFileDiff = withData $ \(params :: Params) -> do
  page <- getPage
  showDiff page page params

showDiff :: String -> String -> Params -> Handler
showDiff file page params = do
  let from = pFrom params
  let to = pTo params
  -- 'to' or 'from' must be given
  when (from == Nothing && to == Nothing) mzero
  fs <- getFileStore
  -- if 'to' is not specified, defaults to current revision
  -- if 'from' is not specified, defaults to revision immediately before 'to'
  from' <- case (from, to) of
              (Just _, _)        -> return from
              (Nothing, Nothing) -> return from
              (Nothing, Just t)  -> do 
                pageHist <- liftIO $ history fs [file]
                                     (TimeRange Nothing Nothing)
                let (_, upto) = break (\r -> idsMatch fs (revId r) t)
                                  pageHist
                return $ if length upto >= 2
                            -- immediately preceding revision
                            then Just $ revId $ upto !! 1
                            else Nothing
  result' <- liftIO $ try $ getDiff fs file from' to
  case result' of
       Left NotFound  -> mzero
       Left e         -> liftIO $ throwIO e
       Right htmlDiff -> formattedPage defaultPageLayout{
                                          pgPageName = page,
                                          pgRevision = from',
                                          pgMessages = pMessages params,
                                          pgTabs = DiffTab :
                                                   pgTabs defaultPageLayout,
                                          pgSelectedTab = DiffTab
                                          }
                                       htmlDiff

getDiff :: FileStore -> FilePath -> Maybe RevisionId -> Maybe RevisionId
        -> IO Html
getDiff fs file from to = do
  rawDiff <- diff fs file from to
  let diffLineToHtml (B, xs) = thespan << unlines xs
      diffLineToHtml (F, xs) = thespan ! [theclass "deleted"] << unlines xs
      diffLineToHtml (S, xs) = thespan ! [theclass "added"]   << unlines xs
  return $ h2 ! [theclass "revision"] <<
             ("Changes from " ++ fromMaybe "beginning" from ++
              " to " ++ fromMaybe "current" to) +++
           pre ! [theclass "diff"] << map diffLineToHtml rawDiff

editPage :: Handler
editPage = withData $ \(params :: Params) -> do
  let rev = pRevision params  -- if this is set, we're doing a revert
  fs <- getFileStore
  page <- getPage
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
  base' <- getWikiBase
  cfg <- getConfig
  let editForm = gui (base' ++ urlForPage page) ! [identifier "editform"] <<
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
                  pgPageName = page,
                  pgMessages = messages,
                  pgRevision = rev,
                  pgShowPageTools = False,
                  pgShowSiteNav = False,
                  pgMarkupHelp = Just $ markupHelp cfg,
                  pgSelectedTab = EditTab,
                  pgScripts = ["preview.js"],
                  pgTitle = ("Editing " ++ page)
                  } editForm

confirmDelete :: Handler
confirmDelete = do
  page <- getPage
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
  formattedPage defaultPageLayout{ pgTitle = "Delete " ++ page ++ "?" } $
    if null fileToDelete
       then ulist ! [theclass "messages"] << li <<
            "There is no file or page by that name."
       else confirmForm

deletePage :: Handler
deletePage = withData $ \(params :: Params) -> do
  page <- getPage
  let file = pFileToDelete params
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete."
                        Just u  -> return (uUsername u, uEmail u)
  let author = Author user email
  let descrip = "Deleted using web interface."
  base' <- getWikiBase
  if pConfirm params && (file == page || file == page <.> "page") 
     then do
       fs <- getFileStore
       liftIO $ delete fs file author descrip
       seeOther (base' ++ "/") $ toResponse $ p << "File deleted"
     else seeOther (base' ++ urlForPage page) $ toResponse $ p << "Not deleted"

updatePage :: Handler
updatePage = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> fail "User must be logged in to delete page."
                        Just u  -> return (uUsername u, uEmail u)
  editedText <- case pEditedText params of
                     Nothing -> error "No body text in POST request"
                     Just b  -> applyPreCommitPlugins b
  let logMsg = pLogMsg params `orIfNull` defaultSummary cfg
  let oldSHA1 = pSHA1 params
  fs <- getFileStore
  base' <- getWikiBase
  if null . filter (not . isSpace) $ logMsg
     then withMessages ["Description cannot be empty."] editPage
     else do
       when (length editedText > fromIntegral (maxPageSize cfg)) $
          error "Page exceeds maximum size."
       -- check SHA1 in case page has been modified, merge
       modifyRes <- if null oldSHA1
                       then liftIO $ create fs (pathForPage page)
                                       (Author user email) logMsg editedText >>
                                     return (Right ())
                       else do
                         expireCachedFile (pathForPage page) `mplus` return ()
                         liftIO $ catch (modify fs (pathForPage page)
                                            oldSHA1 (Author user email) logMsg
                                            editedText)
                                     (\e -> if e == Unchanged
                                               then return (Right ())
                                               else throwIO e)
       case modifyRes of
            Right () -> seeOther (base' ++ urlForPage page) $ toResponse $ p << "Page updated"
            Left (MergeInfo mergedWithRev conflicts mergedText) -> do
               let mergeMsg = "The page has been edited since you checked it out. " ++
                      "Changes from revision " ++ revId mergedWithRev ++
                      " have been merged into your edits below. " ++
                      if conflicts
                         then "Please resolve conflicts and Save."
                         else "Please review and Save."
               withMessages [mergeMsg] $
                 withInput "editedText" mergedText $
                   withInput "sha1" (revId mergedWithRev) editPage

indexPage :: Handler
indexPage = do
  path' <- getPath
  base' <- getWikiBase
  let prefix' = if null path' then "" else path' ++ "/"
  fs <- getFileStore
  listing <- liftIO $ directory fs prefix'
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = False
  let prunedListing = filter (not . isDiscussionPage) listing
  let htmlIndex = fileListToHtml base' prefix' prunedListing
  formattedPage defaultPageLayout{
                  pgPageName = prefix',
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = [],
                  pgTitle = "Contents"} htmlIndex

fileListToHtml :: String -> String -> [Resource] -> Html
fileListToHtml base' prefix files =
  let fileLink (FSFile f) | isPageFile f =
        li ! [theclass "page"  ] <<
          anchor ! [href $ base' ++ urlForPage (prefix ++ dropExtension f)] <<
            dropExtension f
      fileLink (FSFile f) =
        li ! [theclass "upload"] << anchor ! [href $ base' ++ urlForPage (prefix ++ f)] << f
      fileLink (FSDirectory f) =
        li ! [theclass "folder"] <<
          anchor ! [href $ base' ++ urlForPage (prefix ++ f) ++ "/"] << f
      updirs = drop 1 $ inits $ splitPath $ '/' : prefix
      uplink = foldr (\d accum ->
                  concatHtml [ anchor ! [theclass "updir",
                                         href $ if length d <= 1
                                                   then base' ++ "/_index"
                                                   else base' ++
                                                        urlForPage (joinPath $ drop 1 d)] <<
                  lastNote "fileListToHtml" d, accum]) noHtml updirs
  in uplink +++ ulist ! [theclass "index"] << map fileLink files

-- NOTE:  The current implementation of categoryPage does not go via the
-- filestore abstraction.  That is bad, but can only be fixed if we add
-- more sophisticated searching options to filestore.
categoryPage :: Handler
categoryPage = do
  category <- getPath
  cfg <- getConfig
  let repoPath = repositoryPath cfg
  let categoryDescription = "Category: " ++ category
  fs <- getFileStore
  files <- liftIO $ index fs
  let pages = filter (\f -> isPageFile f && not (isDiscussPageFile f)) files
  matches <- liftM catMaybes $
             forM pages $ \f ->
               liftIO (readFileUTF8 $ repoPath </> f) >>= \s ->
               return $ if category `elem` (extractCategories s)
                           then Just $ dropExtension f
                           else Nothing
  base' <- getWikiBase
  let toMatchListItem file = li <<
        [ anchor ! [href $ base' ++ urlForPage (dropExtension file)] << dropExtension file ]
  let htmlMatches = ulist << map toMatchListItem matches
  formattedPage defaultPageLayout{
                  pgPageName = categoryDescription,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = categoryDescription }
                htmlMatches

categoryListPage :: Handler
categoryListPage = do
  cfg <- getConfig
  let repoPath = repositoryPath cfg
  fs <- getFileStore
  files <- liftIO $ index fs
  let pages = filter (\f -> isPageFile f && not (isDiscussPageFile f)) files
  categories <- liftIO $
                liftM (nub . sort . concat) $
                forM pages $ \f ->
                liftM extractCategories (readFileUTF8 (repoPath </> f))
  base' <- getWikiBase
  let toCatLink ctg = li <<
        [ anchor ! [href $ base' ++ "/_category" ++ urlForPage ctg] << ctg ]
  let htmlMatches = ulist << map toCatLink categories
  formattedPage defaultPageLayout{
                  pgPageName = "Categories",
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = "Categories" } htmlMatches

-- | Returns username of logged in user or null string if nobody logged in.
currentUser :: Handler
currentUser = do
  req <- askRq
  ok $ toResponse $ maybe "" toString (getHeader "REMOTE_USER" req)

expireCache :: Handler
expireCache = do
  page <- getPage
  -- try it as a page first, then as an uploaded file
  expireCachedFile (pathForPage page)
  expireCachedFile page
  ok $ toResponse ()

feedHandler :: Handler
feedHandler = do
  cfg <- getConfig
  when (not $ useFeed cfg) mzero
  base' <- getWikiBase
  feedBase <- if null (baseUrl cfg)  -- if baseUrl blank, try to get it from Host header
                 then do
                   mbHost <- getHost
                   case mbHost of
                        Nothing    -> error "Could not determine base URL"
                        Just hn    -> return $ "http://" ++ hn ++ base'
                 else case baseUrl cfg ++ base' of
                           x@('h':'t':'t':'p':':':'/':'/':_) -> return x
                           y                                 -> return $ "http://" ++ y
  let fc = FeedConfig{
              fcTitle = wikiTitle cfg
            , fcBaseUrl = feedBase
            , fcFeedDays = feedDays cfg }
  path' <- getPath     -- e.g. "foo/bar" if they hit /_feed/foo/bar
  let file = (path' `orIfNull` "_site") <.> "feed"
  let mbPath = if null path' then Nothing else Just path'
  -- first, check for a cached version that is recent enough
  now <- liftIO $ getClockTime
  let isRecentEnough t = normalizeTimeDiff (diffClockTimes now t) <
                         normalizeTimeDiff (noTimeDiff{tdMin = fromIntegral $ feedRefreshTime cfg})
  mbCached <- lookupCache file
  case mbCached of
       Just (modtime, contents) | isRecentEnough modtime -> do
            let emptyResponse = setContentType "application/atom+xml; charset=utf-8" . toResponse $ ()
            ok $ emptyResponse{rsBody = B.fromChunks [contents]}
       _ -> do
            fs <- getFileStore
            resp <- liftM toResponse $ liftIO (filestoreToXmlFeed fc fs mbPath)
            cacheContents file $ S.concat $ B.toChunks $ rsBody resp
            ok . setContentType "application/atom+xml; charset=UTF-8" $ resp
