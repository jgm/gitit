{-# LANGUAGE CPP #-}
{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>,
Anton van Straaten <anton@appsolutions.com>

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

{- Functions for content conversion.
-}

module Network.Gitit.ContentTransformer
  (
  -- * ContentTransformer runners
    runPageTransformer
  , runFileTransformer
  -- * Gitit responders
  , showRawPage
  , showFileAsText
  , showPage
  , exportPage
  , showHighlightedSource
  , showFile
  , preview
  , applyPreCommitPlugins
  -- * Cache support for transformers
  , cacheHtml
  , cachedHtml
  -- * Content retrieval combinators
  , rawContents
  -- * Response-generating combinators
  , textResponse
  , mimeFileResponse
  , mimeResponse
  , exportPandoc
  , applyWikiTemplate
  -- * Content-type transformation combinators
  , pageToWikiPandoc
  , pageToPandoc
  , pandocToHtml
  , highlightSource
  -- * Content or context augmentation combinators
  , applyPageTransforms
  , wikiDivify
  , addPageTitleToPandoc
  , addMathSupport
  , addScripts
  -- * ContentTransformer context API
  , getFileName
  , getPageName
  , getLayout
  , getParams
  , getCacheable
  -- * Pandoc and wiki content conversion support
  , inlinesToURL
  , inlinesToString
  )
where

import Control.Exception (throwIO, catch)
import Control.Monad.State
import Control.Monad.Reader (ask)
import Data.Maybe (isNothing, mapMaybe)
import Network.Gitit.Cache (lookupCache, cacheContents)
import Network.Gitit.Export (exportFormats)
import Network.Gitit.Framework
import Network.Gitit.Layout
import Network.Gitit.Page (stringToPage)
import Network.Gitit.Server
import Network.Gitit.State
import Network.Gitit.Types
import Network.URI (isUnescapedInURI)
import Network.URL (encString)
import Prelude hiding (catch)
import System.FilePath
import qualified Text.Pandoc.Builder as B
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Highlighting.Kate
import Text.Pandoc hiding (MathML, WebTeX, MathJax)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
#if MIN_VERSION_blaze_html(0,5,0)
import Text.Blaze.Html.Renderer.String as Blaze ( renderHtml )
#else
import Text.Blaze.Renderer.String as Blaze ( renderHtml )
#endif
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.ByteString as S (concat)
import qualified Data.ByteString.Lazy as L (toChunks, fromChunks)
import qualified Data.FileStore as FS
import qualified Text.Pandoc as Pandoc

--
-- ContentTransformer runners
--

runTransformer :: ToMessage a
               => (String -> String)
               -> ContentTransformer a
               -> GititServerPart a
runTransformer pathFor xform = withData $ \params -> do
  page <- getPage
  cfg <- getConfig
  evalStateT xform  Context{ ctxFile = pathFor page
                           , ctxLayout = defaultPageLayout{
                                             pgPageName = page
                                           , pgTitle = page
                                           , pgPrintable = pPrintable params
                                           , pgMessages = pMessages params
                                           , pgRevision = pRevision params
                                           , pgLinkToFeed = useFeed cfg }
                           , ctxCacheable = True
                           , ctxTOC = tableOfContents cfg
                           , ctxBirdTracks = showLHSBirdTracks cfg
                           , ctxCategories = []
                           , ctxMeta = [] }

-- | Converts a @ContentTransformer@ into a @GititServerPart@;
-- specialized to wiki pages.
runPageTransformer :: ToMessage a
                   => ContentTransformer a
                   -> GititServerPart a
runPageTransformer = runTransformer pathForPage

-- | Converts a @ContentTransformer@ into a @GititServerPart@;
-- specialized to non-pages.
runFileTransformer :: ToMessage a
                   => ContentTransformer a
                   -> GititServerPart a
runFileTransformer = runTransformer id

--
-- Gitit responders
--

-- | Responds with raw page source.
showRawPage :: Handler
showRawPage = runPageTransformer rawTextResponse

-- | Responds with raw source (for non-pages such as source
-- code files).
showFileAsText :: Handler
showFileAsText = runFileTransformer rawTextResponse

-- | Responds with rendered wiki page.
showPage :: Handler
showPage = runPageTransformer htmlViaPandoc

-- | Responds with page exported into selected format.
exportPage :: Handler
exportPage = runPageTransformer exportViaPandoc

-- | Responds with highlighted source code.
showHighlightedSource :: Handler
showHighlightedSource = runFileTransformer highlightRawSource

-- | Responds with non-highlighted source code.
showFile :: Handler
showFile = runFileTransformer (rawContents >>= mimeFileResponse)

-- | Responds with rendered page derived from form data.
preview :: Handler
preview = runPageTransformer $
          liftM (filter (/= '\r') . pRaw) getParams >>=
          contentsToPage >>=
          pageToWikiPandoc >>=
          pandocToHtml >>=
          return . toResponse . renderHtmlFragment

-- | Applies pre-commit plugins to raw page source, possibly
-- modifying it.
applyPreCommitPlugins :: String -> GititServerPart String
applyPreCommitPlugins = runPageTransformer . applyPreCommitTransforms

--
-- Top level, composed transformers
--

-- | Responds with raw source.
rawTextResponse :: ContentTransformer Response
rawTextResponse = rawContents >>= textResponse

-- | Responds with a wiki page in the format specified
-- by the @format@ parameter.
exportViaPandoc :: ContentTransformer Response
exportViaPandoc = rawContents >>=
                  maybe mzero return >>=
                  contentsToPage >>=
                  pageToWikiPandoc >>=
                  exportPandoc

-- | Responds with a wiki page. Uses the cache when
-- possible and caches the rendered page when appropriate.
htmlViaPandoc :: ContentTransformer Response
htmlViaPandoc = cachedHtml `mplus`
                  (rawContents >>=
                   maybe mzero return >>=
                   contentsToPage >>=
                   pageToWikiPandoc >>=
                   addMathSupport >>=
                   pandocToHtml >>=
                   wikiDivify >>=
                   applyWikiTemplate >>=
                   cacheHtml)

-- | Responds with highlighted source code in a wiki
-- page template.  Uses the cache when possible and
-- caches the rendered page when appropriate.
highlightRawSource :: ContentTransformer Response
highlightRawSource =
  cachedHtml `mplus`
    (updateLayout (\l -> l { pgTabs = [ViewTab,HistoryTab] }) >>
     rawContents >>=
     highlightSource >>=
     applyWikiTemplate >>=
     cacheHtml)

--
-- Cache support for transformers
--

-- | Caches a response (actually just the response body) on disk,
-- unless the context indicates that the page is not cacheable.
cacheHtml :: Response -> ContentTransformer Response
cacheHtml resp' = do
  params <- getParams
  file <- getFileName
  cacheable <- getCacheable
  cfg <- lift getConfig
  when (useCache cfg && cacheable && isNothing (pRevision params) && not (pPrintable params)) $
    lift $ cacheContents file $ S.concat $ L.toChunks $ rsBody resp'
  return resp'

-- | Returns cached page if available, otherwise mzero.
cachedHtml :: ContentTransformer Response
cachedHtml = do
  file <- getFileName
  params <- getParams
  cfg <- lift getConfig
  if useCache cfg && not (pPrintable params) && isNothing (pRevision params)
     then do mbCached <- lift $ lookupCache file
             let emptyResponse = setContentType "text/html; charset=utf-8" . toResponse $ ()
             maybe mzero (\(_modtime, contents) -> lift . ok $ emptyResponse{rsBody = L.fromChunks [contents]}) mbCached
     else mzero

--
-- Content retrieval combinators
--

-- | Returns raw file contents.
rawContents :: ContentTransformer (Maybe String)
rawContents = do
  params <- getParams
  file <- getFileName
  fs <- lift getFileStore
  let rev = pRevision params
  liftIO $ catch (liftM Just $ FS.retrieve fs file rev)
                 (\e -> if e == FS.NotFound then return Nothing else throwIO e)

--
-- Response-generating combinators
--

-- | Converts raw contents to a text/plain response.
textResponse :: Maybe String -> ContentTransformer Response
textResponse Nothing  = mzero  -- fail quietly if file not found
textResponse (Just c) = mimeResponse c "text/plain; charset=utf-8"

-- | Converts raw contents to a response that is appropriate with
-- a mime type derived from the page's extension.
mimeFileResponse :: Maybe String -> ContentTransformer Response
mimeFileResponse Nothing = error "Unable to retrieve file contents."
mimeFileResponse (Just c) =
  mimeResponse c =<< lift . getMimeTypeForExtension . takeExtension =<< getFileName

mimeResponse :: Monad m
             => String        -- ^ Raw contents for response body
             -> String        -- ^ Mime type
             -> m Response
mimeResponse c mimeType =
  return . setContentType mimeType . toResponse $ c

-- | Converts Pandoc to response using format specified in parameters.
exportPandoc :: Pandoc -> ContentTransformer Response
exportPandoc doc = do
  params <- getParams
  page <- getPageName
  cfg <- lift getConfig
  let format = pFormat params
  case lookup format (exportFormats cfg) of
       Nothing     -> error $ "Unknown export format: " ++ format
       Just writer -> lift (writer page doc)

-- | Adds the sidebar, page tabs, and other elements of the wiki page
-- layout to the raw content.
applyWikiTemplate :: Html -> ContentTransformer Response
applyWikiTemplate c = do
  Context { ctxLayout = layout } <- get
  lift $ formattedPage layout c

--
-- Content-type transformation combinators
--

-- | Converts Page to Pandoc, applies page transforms, and adds page
-- title.
pageToWikiPandoc :: Page -> ContentTransformer Pandoc
pageToWikiPandoc page' =
  pageToWikiPandoc' page' >>= addPageTitleToPandoc (pageTitle page')

pageToWikiPandoc' :: Page -> ContentTransformer Pandoc
pageToWikiPandoc' = applyPreParseTransforms >=>
                     pageToPandoc >=> applyPageTransforms

-- | Converts source text to Pandoc using default page type.
pageToPandoc :: Page -> ContentTransformer Pandoc
pageToPandoc page' = do
  modifyContext $ \ctx -> ctx{ ctxTOC = pageTOC page'
                             , ctxCategories = pageCategories page'
                             , ctxMeta = pageMeta page' }
  return $ readerFor (pageFormat page') (pageLHS page') (pageText page')

-- | Converts contents of page file to Page object.
contentsToPage :: String -> ContentTransformer Page
contentsToPage s = do
  cfg <- lift getConfig
  pn <- getPageName
  return $ stringToPage cfg pn s

-- | Converts pandoc document to HTML.
pandocToHtml :: Pandoc -> ContentTransformer Html
pandocToHtml pandocContents = do
  base' <- lift getWikiBase
  toc <- liftM ctxTOC get
  bird <- liftM ctxBirdTracks get
  cfg <- lift getConfig
  return $ primHtml $ T.unpack .
           (if xssSanitize cfg then sanitizeBalance else id) . T.pack $
           writeHtmlString def{
                        writerStandalone = True
                      , writerTemplate = "$if(toc)$<div id=\"TOC\">\n$toc$\n</div>\n$endif$\n$body$"
                      , writerHTMLMathMethod =
                            case mathMethod cfg of
                                 MathML -> Pandoc.MathML Nothing
                                 WebTeX u -> Pandoc.WebTeX u
                                 MathJax u -> Pandoc.MathJax u
                                 _      -> JsMath (Just $ base' ++
                                                      "/js/jsMath/easy/load.js")
                      , writerTableOfContents = toc
                      , writerExtensions = if bird
                                              then Set.insert
                                                   Ext_literate_haskell
                                                   $ writerExtensions def
                                              else writerExtensions def
                      -- note: javascript obfuscation gives problems on preview
                      , writerEmailObfuscation = ReferenceObfuscation
                      } pandocContents

-- | Returns highlighted source code.
highlightSource :: Maybe String -> ContentTransformer Html
highlightSource Nothing = mzero
highlightSource (Just source) = do
  file <- getFileName
  let formatOpts = defaultFormatOpts { numberLines = True, lineAnchors = True }
  case languagesByExtension $ takeExtension file of
        []    -> mzero
        (l:_) -> return $ primHtml $ Blaze.renderHtml
                        $ formatHtmlBlock formatOpts
                        $! highlightAs l $ filter (/='\r') source

--
-- Plugin combinators
--

getPageTransforms :: ContentTransformer [Pandoc -> PluginM Pandoc]
getPageTransforms = liftM (mapMaybe pageTransform) $ queryGititState plugins
  where pageTransform (PageTransform x) = Just x
        pageTransform _                 = Nothing

getPreParseTransforms :: ContentTransformer [String -> PluginM String]
getPreParseTransforms = liftM (mapMaybe preParseTransform) $
                          queryGititState plugins
  where preParseTransform (PreParseTransform x) = Just x
        preParseTransform _                     = Nothing

getPreCommitTransforms :: ContentTransformer [String -> PluginM String]
getPreCommitTransforms = liftM (mapMaybe preCommitTransform) $
                          queryGititState plugins
  where preCommitTransform (PreCommitTransform x) = Just x
        preCommitTransform _                      = Nothing

-- | @applyTransform a t@ applies the transform @t@ to input @a@.
applyTransform :: a -> (a -> PluginM a) -> ContentTransformer a
applyTransform inp transform = do
  context <- get
  conf <- lift getConfig
  user <- lift getLoggedInUser
  fs <- lift getFileStore
  req <- lift askRq
  let pluginData = PluginData{ pluginConfig = conf
                             , pluginUser = user
                             , pluginRequest = req
                             , pluginFileStore = fs }
  (result', context') <- liftIO $ runPluginM (transform inp) pluginData context
  put context'
  return result'

-- | Applies all the page transform plugins to a Pandoc document.
applyPageTransforms :: Pandoc -> ContentTransformer Pandoc
applyPageTransforms c = do
  xforms <- getPageTransforms
  foldM applyTransform c (wikiLinksTransform : xforms)

-- | Applies all the pre-parse transform plugins to a Page object.
applyPreParseTransforms :: Page -> ContentTransformer Page
applyPreParseTransforms page' = getPreParseTransforms >>= foldM applyTransform (pageText page') >>=
                                (\t -> return page'{ pageText = t })

-- | Applies all the pre-commit transform plugins to a raw string.
applyPreCommitTransforms :: String -> ContentTransformer String
applyPreCommitTransforms c = getPreCommitTransforms >>= foldM applyTransform c

--
-- Content or context augmentation combinators
--

-- | Puts rendered page content into a wikipage div, adding
-- categories.
wikiDivify :: Html -> ContentTransformer Html
wikiDivify c = do
  categories <- liftM ctxCategories get
  base' <- lift getWikiBase
  let categoryLink ctg = li (anchor ! [href $ base' ++ "/_category/" ++ ctg] << ctg)
  let htmlCategories = if null categories
                          then noHtml
                          else thediv ! [identifier "categoryList"] << ulist << map categoryLink categories
  return $ thediv ! [identifier "wikipage"] << [c, htmlCategories]

-- | Adds page title to a Pandoc document.
addPageTitleToPandoc :: String -> Pandoc -> ContentTransformer Pandoc
addPageTitleToPandoc title' (Pandoc _ blocks) = do
  updateLayout $ \layout -> layout{ pgTitle = title' }
  return $ if null title'
              then Pandoc nullMeta blocks
              else Pandoc (B.setMeta "title" (B.str title') nullMeta) blocks

-- | Adds javascript links for math support.
addMathSupport :: a -> ContentTransformer a
addMathSupport c = do
  conf <- lift getConfig
  updateLayout $ \l ->
    case mathMethod conf of
         JsMathScript -> addScripts l ["jsMath/easy/load.js"]
         MathML       -> addScripts l ["MathMLinHTML.js"]
         WebTeX _     -> l
         MathJax u    -> addScripts l [u]
         RawTeX       -> l
  return c

-- | Adds javascripts to page layout.
addScripts :: PageLayout -> [String] -> PageLayout
addScripts layout scriptPaths =
  layout{ pgScripts = scriptPaths ++ pgScripts layout }

--
-- ContentTransformer context API
--

getParams :: ContentTransformer Params
getParams = lift (withData return)

getFileName :: ContentTransformer FilePath
getFileName = liftM ctxFile get

getPageName :: ContentTransformer String
getPageName = liftM (pgPageName . ctxLayout) get

getLayout :: ContentTransformer PageLayout
getLayout = liftM ctxLayout get

getCacheable :: ContentTransformer Bool
getCacheable = liftM ctxCacheable get

-- | Updates the layout with the result of applying f to the current layout
updateLayout :: (PageLayout -> PageLayout) -> ContentTransformer ()
updateLayout f = do
  ctx <- get
  let l = ctxLayout ctx
  put ctx { ctxLayout = f l }

--
-- Pandoc and wiki content conversion support
--

readerFor :: PageType -> Bool -> String -> Pandoc
readerFor pt lhs =
  let defPS = def{ readerSmart = True
                 , readerExtensions = if lhs
                                         then Set.insert Ext_literate_haskell
                                              $ readerExtensions def
                                         else readerExtensions def }
  in case pt of
       RST      -> readRST defPS
       Markdown -> readMarkdown defPS
       LaTeX    -> readLaTeX defPS
       HTML     -> readHtml defPS
       Textile  -> readTextile defPS

wikiLinksTransform :: Pandoc -> PluginM Pandoc
wikiLinksTransform pandoc
  = do cfg <- liftM pluginConfig ask -- Can't use askConfig from Interface due to circular dependencies.
       return (bottomUp (convertWikiLinks cfg) pandoc)

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Config -> Inline -> Inline
convertWikiLinks cfg (Link ref ("", "")) | useAbsoluteUrls cfg =
  Link ref (baseUrl cfg </> inlinesToURL ref, "Go to wiki page")
convertWikiLinks _cfg (Link ref ("", "")) =
  Link ref (inlinesToURL ref, "Go to wiki page")
convertWikiLinks _cfg x = x

-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL = encString False isUnescapedInURI . inlinesToString

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = concatMap go
  where go x = case x of
               Str s                   -> s
               Emph xs                 -> concatMap go xs
               Strong xs               -> concatMap go xs
               Strikeout xs            -> concatMap go xs
               Superscript xs          -> concatMap go xs
               Subscript xs            -> concatMap go xs
               SmallCaps xs            -> concatMap go xs
               Quoted DoubleQuote xs   -> '"' : (concatMap go xs ++ "\"")
               Quoted SingleQuote xs   -> '\'' : (concatMap go xs ++ "'")
               Cite _ xs               -> concatMap go xs
               Code _ s                -> s
               Space                   -> " "
               LineBreak               -> " "
               Math DisplayMath s      -> "$$" ++ s ++ "$$"
               Math InlineMath s       -> "$" ++ s ++ "$"
               RawInline (Format "tex") s -> s
               RawInline _ _           -> ""
               Link xs _               -> concatMap go xs
               Image xs _              -> concatMap go xs
               Note _                  -> ""
               Span _ xs               -> concatMap go xs

