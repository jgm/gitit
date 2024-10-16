{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
                      , updatePage
                      , editPage
                      , deletePage
                      , confirmDelete
                      , showHighlightedSource
                      , expireCache
                      , feedHandler
                      )
where
import Safe
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Types
import Network.Gitit.Feed (filestoreToXmlFeed, FeedConfig(..))
import Network.Gitit.Util (orIfNull)
import Network.Gitit.Cache (expireCachedFile, lookupCache, cacheContents)
import Network.Gitit.ContentTransformer (showRawPage, showFileAsText, showPage,
        showHighlightedSource, preview, applyPreCommitPlugins)
import Network.Gitit.Page (readCategories)
import qualified Control.Exception as E
import System.FilePath
import Network.Gitit.State
import Data.List (intercalate, intersperse, delete, nub, sortBy, find, isPrefixOf, inits, sort, (\\))
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)
import Data.Ord (comparing)
import Data.Char (toLower, isSpace)
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import Network.HTTP (urlEncodeVars)
import Data.Time (getCurrentTime, addUTCTime)
import Data.Time.Clock (diffUTCTime, UTCTime(..))
import Data.FileStore
import System.Log.Logger (logM, Priority(..))
import Text.Blaze.Html.Renderer.String as Blaze ( renderHtml )
import Text.Blaze.Html5 hiding (b, search, u, s, contents, source, html, title, map)
import Text.Blaze.Html5.Attributes hiding (span, id)
import qualified Text.Blaze.Html5 as Html5 hiding (search)
import qualified Text.Blaze.Html5.Attributes as Html5.Attr hiding (span)
import Data.String (IsString(fromString))
import Prelude hiding (span)

handleAny :: Handler
handleAny = withData $ \(params :: Params) -> uriRest $ \uri ->
  let path' = uriPath uri
  in  do fs <- getFileStore
         let rev = pRevision params
         mimetype <- getMimeTypeForExtension
                      (takeExtension path')
         res <- liftIO $ E.try
                (retrieve fs path' rev :: IO B.ByteString)
         case res of
                Right contents -> ignoreFilters >>  -- don't compress
                                  (ok $ setContentType mimetype $
                                    (toResponse (renderHtml mempty)) {rsBody = contents})
                                    -- ugly hack
                Left NotFound  -> mzero
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
  base' <- getWikiBase
  prunedFiles <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  let pages = map dropExtension prunedFiles
  if null pages
     then error "No pages found!"
     else do
       secs <- liftIO (fmap utctDayTime getCurrentTime)
       let newPage = pages !!
                     (truncate (secs * 1000000) `mod` length pages)
       seeOther (base' ++ urlForPage newPage) $ toResponse $
         renderHtml $ p $ "Redirecting to a random page"

discussPage :: Handler
discussPage = do
  page <- getPage
  base' <- getWikiBase
  seeOther (base' ++ urlForPage (if isDiscussPage page then page else ('@':page))) $
                     toResponse ("Redirecting to discussion page" :: String)

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
                                    }
                    $ p $ mconcat
                      [ fromString
                          $ "There is no page named '" ++ page ++ "'. You can:"
                      , (ul $ mconcat
                          [ li $ a !
                                href (fromString $ base' ++ "/_edit" ++ urlForPage page)
                                  $ fromString ("Create the page '" ++ page ++ "'")
                          , li $ a !
                                href (fromString $ base' ++ "/_search?" ++
                                    (urlEncodeVars [("patterns", page)]))
                                  $ fromString ("Search for pages containing the text '" ++
                                    page ++ "'")])
                      ]

fileInput :: AttributeValue -> AttributeValue -> Html
fileInput nameAndId val =  input ! type_ "file" ! Html5.Attr.id nameAndId ! name nameAndId ! value val
textfieldInput :: AttributeValue -> AttributeValue -> Html
textfieldInput nameAndId val = input ! type_ "text" ! Html5.Attr.id nameAndId ! name nameAndId ! value val
checkboxInput :: AttributeValue -> AttributeValue -> Html
checkboxInput nameAndId val = input ! type_ "checkbox" ! Html5.Attr.id nameAndId ! name nameAndId ! value val
submitInput :: AttributeValue -> AttributeValue -> Html
submitInput nameAndId val = input ! type_ "submit" ! Html5.Attr.id nameAndId ! name nameAndId ! value val

uploadForm :: Handler
uploadForm = withData $ \(params :: Params) -> do
  let origPath = pFilename params
  let wikiname = pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  let upForm = Html5.form ! Html5.Attr.method "post" ! enctype "multipart/form-data"
       $ fieldset $ mconcat
       [ p $ mconcat
              [ Html5.label ! for "file" $ "File to upload:"
              , br
              , fileInput "file" (fromString origPath) ]
       , p $ mconcat
              [ Html5.label ! for "wikiname" $ "Name on wiki, including extension"
              , noscript $ " (leave blank to use the same filename)"
              , ":"
              , br
              , textfieldInput "wikiname" (fromString wikiname)
              , preEscapedString "&nbsp;"
              , checkboxInput "overwrite" "yes"
              , Html5.label ! for "overwrite" $ "Overwrite existing file"
              ]
       , p $ mconcat
              [ Html5.label ! for "logMsg" $ "Description of content or changes:"
              , br
              , textfieldInput "logMsg" (fromString logMsg) ! size "60"
              , submitInput "upload" "Upload" ]
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
  let filePath = pFilePath params
  let wikiname = normalise
                 $ dropWhile (=='/')
                 $ pWikiname params `orIfNull` takeFileName origPath
  let logMsg = pLogMsg params
  cfg <- getConfig
  wPF <- isPageFile wikiname
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
                        Just u  -> return (uUsername u, uEmail u)
  let overwrite = pOverwrite params
  fs <- getFileStore
  exists <- liftIO $ E.catch (latest fs wikiname >> return True) $ \e ->
                      if e == NotFound
                         then return False
                         else E.throwIO e >> return True
  let inStaticDir = staticDir cfg `isPrefixOf` (repositoryPath cfg </> wikiname)
  let inTemplatesDir = templatesDir cfg `isPrefixOf` (repositoryPath cfg </> wikiname)
  let dirs' = splitDirectories $ takeDirectory wikiname
  let imageExtensions = [".png", ".jpg", ".gif"]
  let errors = validate
                 [ (null . filter (not . isSpace) $ logMsg,
                    "Description cannot be empty.")
                 , (".." `elem` dirs', "Wikiname cannot contain '..'")
                 , (null origPath, "File not found.")
                 , (inStaticDir,  "Destination is inside static directory.")
                 , (inTemplatesDir,  "Destination is inside templates directory.")
                 , (not overwrite && exists, "A file named '" ++ wikiname ++
                    "' already exists in the repository: choose a new name " ++
                    "or check the box to overwrite the existing file.")
                 , (wPF,
                    "This file extension is reserved for wiki pages.")
                 ]
  if null errors
     then do
       expireCachedFile wikiname `mplus` return ()
       fileContents <- liftIO $ B.readFile filePath
       let len = B.length fileContents
       liftIO $ save fs wikiname (Author user email) logMsg fileContents
       let contents = Html5.div $ mconcat
             [ h2 $ fromString ("Uploaded " ++ show len ++ " bytes")
             , if takeExtension wikiname `elem` imageExtensions
                  then (p $ "To add this image to a page, use:") <>
                       (pre $ fromString ("![alt text](/" ++ wikiname ++ ")"))
                  else (p $ "To link to this resource from a page, use:") <>
                       (pre $ fromString ("[link label](/" ++ wikiname ++ ")")) ]
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
  pruned_files <- liftIO (index fs) >>= filterM isPageFile
  let allPageNames = map dropExtension pruned_files
  let findPage f = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = (map toLower gotopage) == (map toLower f)
  let prefixMatch f = (map toLower gotopage) `isPrefixOf` (map toLower f)
  base' <- getWikiBase
  case findPage exactMatch of
       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                     ("Redirecting to exact match" :: String)
       Nothing -> case findPage insensitiveMatch of
                       Just m  -> seeOther (base' ++ urlForPage m) $ toResponse
                                    ("Redirecting to case-insensitive match" :: String)
                       Nothing -> case findPage prefixMatch of
                                       Just m  -> seeOther (base' ++ urlForPage m) $
                                                  toResponse $ "Redirecting" ++
                                                    " to partial match"
                                       Nothing -> searchResults

searchResults :: Handler
searchResults = withData $ \(params :: Params) -> do
  let patterns = pPatterns params `orIfNull` [pGotoPage params]
  fs <- getFileStore
  matchLines <- if null patterns
                   then return []
                   else liftIO $ E.catch (search fs SearchQuery{
                                                  queryPatterns = patterns
                                                , queryWholeWords = True
                                                , queryMatchAll = True
                                                , queryIgnoreCase = True })
                                       -- catch error, because newer versions of git
                                       -- return 1 on no match, and filestore <=0.3.3
                                       -- doesn't handle this properly:
                                       (\(_ :: FileStoreError)  -> return [])
  let contentMatches = map matchResourceName matchLines
  allPages <- liftIO (index fs) >>= filterM isPageFile
  let slashToSpace = map (\c -> if c == '/' then ' ' else c)
  let inPageName pageName' x = x `elem` (words $ slashToSpace $ dropExtension pageName')
  let matchesPatterns pageName' = not (null patterns) &&
       all (inPageName (map toLower pageName')) (map (map toLower) patterns)
  let pageNameMatches = filter matchesPatterns allPages
  prunedFiles <- filterM isPageFile (contentMatches ++ pageNameMatches)
  let allMatchedFiles = nub $ prunedFiles
  let matchesInFile f =  mapMaybe (\x -> if matchResourceName x == f
                                            then Just (matchLine x)
                                            else Nothing) matchLines
  let matches = map (\f -> (f, matchesInFile f)) allMatchedFiles
  let relevance (f, ms) = length ms + if f `elem` pageNameMatches
                                         then 100
                                         else 0
  let preamble = if null patterns
                    then h3 $ "Please enter a search term."
                    else h3 $ mconcat
                            [ fromString (show (length matches) ++ " matches found for ")
                            , Html5.span ! Html5.Attr.id "pattern" $ fromString $ unwords patterns ]
  base' <- getWikiBase
  let toMatchListItem (file, contents) = li $ mconcat
        [ a ! href (fromString $ base' ++ urlForPage (dropExtension file)) $ fromString $ dropExtension file
        , fromString (" (" ++ show (length contents) ++ " matching lines)")
        , " "
        , a ! href "#" ! class_ "showmatch" !
                    Html5.Attr.style "display: none;" $ if length contents > 0
                                                     then "[show matches]"
                                                     else ""
        , pre ! class_ "matches" $ fromString $ unlines contents]
  let htmlMatches = preamble <>
                    (ol $ foldMap toMatchListItem
                             (reverse $ sortBy (comparing relevance) matches))
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
  cfg <- getConfig
  showHistory (pathForPage page $ defaultExtension cfg) page params

showFileHistory :: Handler
showFileHistory = withData $ \(params :: Params) -> do
  file <- getPage
  showHistory file file params

intDataAttribute :: Tag -> Int -> Attribute
intDataAttribute tag = dataAttribute tag . fromString . show

showHistory :: String -> String -> Params -> Handler
showHistory file page params =  do
  fs <- getFileStore
  hist <- liftIO $ history fs [file] (TimeRange Nothing Nothing)
            (Just $ pLimit params)
  base' <- getWikiBase
  let versionToHtml rev pos = li ! class_ "difflink" ! intDataAttribute "order" pos !
                                    dataAttribute "revision" (fromString $ revId rev) !
                                    dataAttribute "diffurl" (fromString $ base' ++ "/_diff/" ++ page)
        $ mconcat
        [ span ! class_ "date" $ (fromString $ show $ revDateTime rev)
        , " ("
        , span ! class_ "author" $ a ! href (fromString $ base' ++ "/_activity?" ++
            urlEncodeVars [("forUser", authorName $ revAuthor rev)]) $
              fromString (authorName $ revAuthor rev)
        , "): "
        , a ! href (fromString $ base' ++ urlForPage page ++ "?revision=" ++ revId rev) $
           span ! class_ "subject" $ fromString $ revDescription rev
        , noscript $ mconcat
            ([ " [compare with "
             , a ! href (fromString $ base' ++ "/_diff" ++ urlForPage page ++ "?to=" ++ revId rev) $
                  "previous" ] ++
             (if pos /= 1
                  then [ preEscapedString "&nbsp;"
                       , preEscapedString "&bull;"
                       , preEscapedString "&nbsp;"
                       , a ! href (fromString $ base' ++ "/_diff" ++ urlForPage page ++ "?from=" ++
                                  revId rev) $ "current"
                       ]
                  else []) ++
             ["]"])
        ]
  let contents = if null hist
                    then mempty
                    else ul ! class_ "history" $ mconcat $
                           zipWith versionToHtml hist
                           [length hist, (length hist - 1)..1]
  let more = if length hist == pLimit params
                then a ! href (fromString $ base' ++ "/_history" ++ urlForPage page
                                 ++ "?limit=" ++ show (pLimit params + 100)) $
                                 "Show more..."
                else mempty
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
                   } $ contents <> more

showActivity :: Handler
showActivity = withData $ \(params :: Params) -> do
  cfg <- getConfig
  currTime <- liftIO getCurrentTime
  let defaultDaysAgo = fromIntegral (recentActivityDays cfg)
  let daysAgo = addUTCTime (defaultDaysAgo * (-60) * 60 * 24) currTime
  let since = case pSince params of
                   Nothing -> Just daysAgo
                   Just t  -> Just t
  let forUser = pForUser params
  fs <- getFileStore
  hist <- liftIO $ history fs [] (TimeRange since Nothing)
                     (Just $ pLimit params)
  let hist' = case forUser of
                   Nothing -> hist
                   Just u  -> filter (\r -> authorName (revAuthor r) == u) hist
  let fileFromChange (Added f)    = f
      fileFromChange (Modified f) = f
      fileFromChange (Deleted f)  = f
  base' <- getWikiBase
  let fileAnchor revis file = if takeExtension file == "." ++ (defaultExtension cfg)
        then a ! href (fromString $ base' ++ "/_diff" ++ urlForPage (dropExtension file) ++ "?to=" ++ revis) $ fromString $ dropExtension file
        else a ! href (fromString $ base' ++ urlForPage file ++ "?revision=" ++ revis) $ fromString file
  let filesFor changes revis = intersperse " " $
        map (fileAnchor revis . fileFromChange) changes
  let heading = h1 $ fromString ("Recent changes by " ++ fromMaybe "all users" forUser)
  let revToListItem rev = li $ mconcat
        [ span ! class_ "date" $ fromString $ (show $ revDateTime rev)
        , " ("
        , span ! class_ "author" $
            a ! href (fromString $ base' ++ "/_activity?" ++
              urlEncodeVars [("forUser", authorName $ revAuthor rev)]) $
                fromString (authorName $ revAuthor rev)
        , "): "
        , span ! class_ "subject" $ fromString $ revDescription rev
        , " ("
        , span ! class_ "files" $ mconcat $ filesFor (revChanges rev) (revId rev)
        , ")"
        ]
  let contents = ul ! class_ "history" $ foldMap revToListItem hist'
  formattedPage defaultPageLayout{
                  pgMessages = pMessages params,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgTitle = "Recent changes"
                  } (heading <> contents)

showPageDiff :: Handler
showPageDiff = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  showDiff (pathForPage page $ defaultExtension cfg) page params

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
                                     Nothing
                let (_, upto) = break (\r -> idsMatch fs (revId r) t)
                                  pageHist
                return $ if length upto >= 2
                            -- immediately preceding revision
                            then Just $ revId $ upto !! 1
                            else Nothing
  result' <- liftIO $ E.try $ getDiff fs file from' to
  case result' of
       Left NotFound  -> mzero
       Left e         -> liftIO $ E.throwIO e
       Right htmlDiff -> formattedPage defaultPageLayout{
                                          pgPageName = page,
                                          pgRevision = from' `mplus` to,
                                          pgMessages = pMessages params,
                                          pgTabs = DiffTab :
                                                   pgTabs defaultPageLayout,
                                          pgSelectedTab = DiffTab,
                                          pgTitle = page
                                          }
                                       htmlDiff

getDiff :: FileStore -> FilePath -> Maybe RevisionId -> Maybe RevisionId
        -> IO Html
getDiff fs file from to = do
  rawDiff <- diff fs file from to
  let diffLineToHtml (Both xs _) = span $ fromString $ unlines xs
      diffLineToHtml (First xs) = span ! class_ "deleted" $ fromString $ unlines xs
      diffLineToHtml (Second xs) = span ! class_ "added"  $ fromString $ unlines xs
  return $ h2 ! class_ "revision" $
            (fromString $ "Changes from " ++ fromMaybe "beginning" from ++
              " to " ++ fromMaybe "current" to) <>
           (pre ! class_ "diff" $ foldMap diffLineToHtml rawDiff)

editPage :: Handler
editPage = withData editPage'

gui :: AttributeValue -> Html -> Html
gui act = Html5.form ! action act ! Html5.Attr.method "post"

editPage' :: Params -> Handler
editPage' params = do
  let rev = pRevision params  -- if this is set, we're doing a revert
  fs <- getFileStore
  page <- getPage
  cfg <- getConfig
  let getRevisionAndText = E.catch
        (do c <- liftIO $ retrieve fs (pathForPage page $ defaultExtension cfg) rev
            -- even if pRevision is set, we return revId of latest
            -- saved version (because we're doing a revert and
            -- we don't want gitit to merge the changes with the
            -- latest version)
            r <- liftIO $ latest fs (pathForPage page $ defaultExtension cfg) >>= revision fs
            return (Just $ revId r, c))
        (\e -> if e == NotFound
                  then return (Nothing, "")
                  else E.throwIO e)
  (mbRev, raw) <- case pEditedText params of
                         Nothing -> liftIO getRevisionAndText
                         Just t  -> let r = if null (pSHA1 params)
                                               then Nothing
                                               else Just (pSHA1 params)
                                    in return (r, t)
  let messages = pMessages params
  let logMsg = pLogMsg params
  let sha1Box = case mbRev of
                 Just r  -> textfieldInput "sha1" (fromString r) ! Html5.Attr.style "display: none"
                 Nothing -> mempty
  let readonly' = if isJust (pRevision params)
                    -- disable editing of text box if it's a revert
                    then (Html5.Attr.readonly "readonly")
                          <> Html5.Attr.style "color: gray"
                    else mempty
  base' <- getWikiBase
  let editForm = gui (fromString $ base' ++ urlForPage page) ! Html5.Attr.id "editform"
                   $ mconcat
                   [ sha1Box
                   , textarea ! readonly' ! cols "80" ! name "editedText" !
                                  Html5.Attr.id "editedText" $ fromString raw
                   , br
                   , Html5.label ! for "logMsg" $ "Description of changes:"
                   , br
                   , textfieldInput "logMsg"  (fromString $ logMsg `orIfNull` defaultSummary cfg) ! readonly'
                   , submitInput "update" "Save"
                   , preEscapedString "&nbsp;"
                   , submitInput "cancel" "Discard"
                   , preEscapedString "&nbsp;"
                   , input ! type_ "button" ! class_ "editButton"
                           ! Html5.Attr.id "previewButton"
                           ! onclick "updatePreviewPane();"
                           ! Html5.Attr.style "display: none;"
                           ! value "Preview"
                   , Html5.div ! Html5.Attr.id "previewpane" $ mempty
                   ]
  let pgScripts' = ["preview.js"]
  let pgScripts'' = case mathMethod cfg of
       MathML       -> "MathMLinHTML.js" : pgScripts'
       MathJax url  -> url : pgScripts'
       _            -> pgScripts'
  formattedPage defaultPageLayout{
                  pgPageName = page,
                  pgMessages = messages,
                  pgRevision = rev,
                  pgShowPageTools = False,
                  pgShowSiteNav = False,
                  pgMarkupHelp = Just $ markupHelp cfg,
                  pgSelectedTab = EditTab,
                  pgScripts = pgScripts'',
                  pgTitle = ("Editing " ++ page)
                  } editForm

confirmDelete :: Handler
confirmDelete = do
  page <- getPage
  fs <- getFileStore
  cfg <- getConfig
  -- determine whether there is a corresponding page, and if not whether there
  -- is a corresponding file
  pageTest <- liftIO $ E.try $ latest fs (pathForPage page $ defaultExtension cfg)
  fileToDelete <- case pageTest of
                       Right _        -> return $ pathForPage page $ defaultExtension cfg -- a page
                       Left  NotFound -> do
                         fileTest <- liftIO $ E.try $ latest fs page
                         case fileTest of
                              Right _       -> return page  -- a source file
                              Left NotFound -> return ""
                              Left e        -> fail (show e)
                       Left e        -> fail (show e)
  let confirmForm = gui "" $ mconcat
        [ p $ "Are you sure you want to delete this page?"
        , input ! type_ "text" ! name "filetodelete"
                ! Html5.Attr.style "display: none;" ! value (fromString fileToDelete)
        , submitInput "confirm" "Yes, delete it!"
        , " "
        , submitInput "cancel" "No, keep it!"
        , br ]
  formattedPage defaultPageLayout{ pgTitle = "Delete " ++ page ++ "?" } $
    if null fileToDelete
       then ul ! class_ "messages" $ li $
            "There is no file or page by that name."
       else confirmForm

deletePage :: Handler
deletePage = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  let file = pFileToDelete params
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
                        Just u  -> return (uUsername u, uEmail u)
  let author = Author user email
  let descrip = deleteSummary cfg
  base' <- getWikiBase
  if pConfirm params && (file == page || file == page <.> (defaultExtension cfg))
     then do
       fs <- getFileStore
       liftIO $ Data.FileStore.delete fs file author descrip
       seeOther (base' ++ "/") $ toResponse $ p $ "File deleted"
     else seeOther (base' ++ urlForPage page) $ toResponse $ p $ "Not deleted"

updatePage :: Handler
updatePage = withData $ \(params :: Params) -> do
  page <- getPage
  cfg <- getConfig
  mbUser <- getLoggedInUser
  (user, email) <- case mbUser of
                        Nothing -> return ("Anonymous", "")
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
                       then liftIO $ create fs (pathForPage page $ defaultExtension cfg)
                                       (Author user email) logMsg editedText >>
                                     return (Right ())
                       else do
                         expireCachedFile (pathForPage page $ defaultExtension cfg) `mplus` return ()
                         liftIO $ E.catch (modify fs (pathForPage page $ defaultExtension cfg)
                                            oldSHA1 (Author user email) logMsg
                                            editedText)
                                     (\e -> if e == Unchanged
                                               then return (Right ())
                                               else E.throwIO e)
       case modifyRes of
            Right () -> seeOther (base' ++ urlForPage page) $ toResponse $ p $ "Page updated"
            Left (MergeInfo mergedWithRev conflicts mergedText) -> do
               let mergeMsg = "The page has been edited since you checked it out. " ++
                      "Changes from revision " ++ revId mergedWithRev ++
                      " have been merged into your edits below. " ++
                      if conflicts
                         then "Please resolve conflicts and Save."
                         else "Please review and Save."
               editPage' $
                 params{ pEditedText = Just mergedText,
                         pSHA1       = revId mergedWithRev,
                         pMessages   = [mergeMsg] }

indexPage :: Handler
indexPage = do
  path' <- getPath
  base' <- getWikiBase
  cfg <- getConfig
  let ext = defaultExtension cfg
  let prefix' = if null path' then "" else path' ++ "/"
  fs <- getFileStore
  listing <- liftIO $ directory fs prefix'
  let isNotDiscussionPage (FSFile f) = isNotDiscussPageFile f
      isNotDiscussionPage (FSDirectory _) = return True
  prunedListing <- filterM isNotDiscussionPage listing
  let htmlIndex = fileListToHtml base' prefix' ext prunedListing
  formattedPage defaultPageLayout{
                  pgPageName = prefix',
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = [],
                  pgTitle = "Contents"} htmlIndex

fileListToHtml :: String -> String -> String -> [Resource] -> Html
fileListToHtml base' prefix ext files =
  let fileLink (FSFile f) | takeExtension f == "." ++ ext =
        li ! class_ "page" $
          a ! href (fromString $ base' ++ urlForPage (prefix ++ dropExtension f)) $
            fromString $ dropExtension f
      fileLink (FSFile f) = li ! class_ "upload" $ mconcat
        [ a ! href (fromString $ base' ++ urlForPage (prefix ++ f)) $ fromString f
        , a ! href (fromString $ base' ++ "_delete" ++ urlForPage (prefix ++ f)) $ "(delete)"
        ]
      fileLink (FSDirectory f) =
        li ! class_ "folder" $
          a ! href (fromString $ base' ++ urlForPage (prefix ++ f) ++ "/") $ fromString f
      updirs = drop 1 $ inits $ splitPath $ '/' : prefix
      uplink = foldr (\d accum ->
                  mconcat [ a ! class_ "updir" !
                                         href (fromString $ if length d <= 1
                                                   then base' ++ "/_index"
                                                   else base' ++
                                                        urlForPage (joinPath $ drop 1 d)) $
                  fromString $ lastNote "fileListToHtml" d, accum]) mempty updirs
  in uplink <> (ul ! class_ "index" $ foldMap fileLink files)

-- NOTE:  The current implementation of categoryPage does not go via the
-- filestore abstraction.  That is bad, but can only be fixed if we add
-- more sophisticated searching options to filestore.
categoryPage :: Handler
categoryPage = do
  path' <- getPath
  cfg <- getConfig
  let pcategories = wordsBy (==',') path'
  let repoPath = repositoryPath cfg
  let categoryDescription = "Category: " ++ (intercalate " + " pcategories)
  fs <- getFileStore
  pages <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  matches <- liftM catMaybes $
             forM pages $ \f -> do
               categories <- liftIO $ readCategories $ repoPath </> f
               return $ if all ( `elem` categories) pcategories
                           then Just (f, categories \\ pcategories)
                           else Nothing
  base' <- getWikiBase
  let toMatchListItem file = li $
        a ! href (fromString $ base' ++ urlForPage (dropExtension file)) $ fromString $ dropExtension file
  let toRemoveListItem cat = li $
        a ! href (fromString $ base' ++
        (if null (tail pcategories)
         then "/_categories"
         else "/_category" ++ urlForPage (intercalate "," $ Data.List.delete cat pcategories)))
        $ fromString ("-" ++ cat)
  let toAddListItem cat = li $
        a ! href (fromString $ base' ++
          "/_category" ++ urlForPage (path' ++ "," ++ cat))
        $ fromString ("+" ++ cat)
  let matchList = ul $ foldMap toMatchListItem (fst $ unzip matches) <>
                  (Html5.div ! Html5.Attr.id "categoryList" $
                  ul $ mconcat $ (++) (map toAddListItem (nub $ concat $ snd $ unzip matches))
                                (map toRemoveListItem pcategories))
  formattedPage defaultPageLayout{
                  pgPageName = categoryDescription,
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = categoryDescription }
                matchList

categoryListPage :: Handler
categoryListPage = do
  cfg <- getConfig
  let repoPath = repositoryPath cfg
  fs <- getFileStore
  pages <- liftIO (index fs) >>= filterM isPageFile >>= filterM isNotDiscussPageFile
  categories <- liftIO $ liftM (nub . sort . concat) $ forM pages $ \f ->
                  readCategories (repoPath </> f)
  base' <- getWikiBase
  let toCatLink ctg = li $
        a ! href (fromString $ base' ++ "/_category" ++ urlForPage ctg) $ (fromString ctg)
  let htmlMatches = ul $ foldMap toCatLink categories
  formattedPage defaultPageLayout{
                  pgPageName = "Categories",
                  pgShowPageTools = False,
                  pgTabs = [],
                  pgScripts = ["search.js"],
                  pgTitle = "Categories" } htmlMatches

expireCache :: Handler
expireCache = do
  page <- getPage
  cfg <- getConfig
  -- try it as a page first, then as an uploaded file
  expireCachedFile (pathForPage page $ defaultExtension cfg)
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
                           w@('h':'t':'t':'p':'s':':':'/':'/':_) -> return w
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
  now <- liftIO getCurrentTime
  let isRecentEnough t = truncate (diffUTCTime now t) < 60 * feedRefreshTime cfg
  mbCached <- lookupCache file
  case mbCached of
       Just (modtime, contents) | isRecentEnough modtime -> do
            let emptyResponse = setContentType "application/atom+xml; charset=utf-8" . toResponse $ ()
            ok $ emptyResponse{rsBody = B.fromChunks [contents]}
       _ -> do
            fs <- getFileStore
            resp' <- liftM toResponse $ liftIO (filestoreToXmlFeed fc fs mbPath)
            cacheContents file $ S.concat $ B.toChunks $ rsBody resp'
            ok . setContentType "application/atom+xml; charset=UTF-8" $ resp'
