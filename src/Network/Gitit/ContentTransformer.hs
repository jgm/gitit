{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Control.Exception as E
import Control.Monad.State
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Data.List (stripPrefix)
import Data.Maybe (isNothing, mapMaybe)
import Data.Semigroup ((<>))
import Network.Gitit.Cache (lookupCache, cacheContents)
import Network.Gitit.Export (exportFormats)
import Network.Gitit.Framework hiding (uriPath)
import Network.Gitit.Layout
import Network.Gitit.Page (stringToPage)
import Network.Gitit.Server
import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Util (getPageTypeDefaultExtensions)
import Network.HTTP (urlDecode)
import Network.URI (isUnescapedInURI)
import Network.URL (encString)
import System.FilePath
import qualified Text.Pandoc.Builder as B
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Skylighting hiding (Context)
import Text.Pandoc hiding (MathML, WebTeX, MathJax)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import Text.XHtml.Strict (stringToHtmlString)
#if MIN_VERSION_blaze_html(0,5,0)
import Text.Blaze.Html.Renderer.String as Blaze ( renderHtml )
#else
import Text.Blaze.Renderer.String as Blaze ( renderHtml )
#endif
import URI.ByteString (Query(Query), URIRef(uriPath), laxURIParserOptions,
                       parseURI, uriQuery)
import qualified Data.Text as T
import qualified Data.ByteString as S (concat)
import qualified Data.ByteString.Char8 as SC (pack, unpack)
import qualified Data.ByteString.Lazy as L (toChunks, fromChunks)
import qualified Data.FileStore as FS
import qualified Text.Pandoc as Pandoc

--
-- ContentTransformer runners
--

runPageTransformer :: ToMessage a
               => ContentTransformer a
               -> GititServerPart a
runPageTransformer xform = withData $ \params -> do
  page <- getPage
  cfg <- getConfig
  evalStateT xform  Context{ ctxFile = pathForPage page (defaultExtension cfg)
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

runFileTransformer :: ToMessage a
               => ContentTransformer a
               -> GititServerPart a
runFileTransformer xform = withData $ \params -> do
  page <- getPage
  cfg <- getConfig
  evalStateT xform  Context{ ctxFile = id page
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
-- runPageTransformer :: ToMessage a
--                    => ContentTransformer a
--                    -> GititServerPart a
-- runPageTransformer = runTransformer pathForPage

-- | Converts a @ContentTransformer@ into a @GititServerPart@;
-- specialized to non-pages.
-- runFileTransformer :: ToMessage a
--                    => ContentTransformer a
--                    -> GititServerPart a
-- runFileTransformer = runTransformer id

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
                   handleRedirects >>=
                   either return
                     (pageToWikiPandoc >=>
                      addMathSupport >=>
                      pandocToHtml >=>
                      wikiDivify >=>
                      applyWikiTemplate >=>
                      cacheHtml))

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
  liftIO $ E.catch (liftM Just $ FS.retrieve fs file rev)
               (\e -> if e == FS.NotFound then return Nothing else E.throwIO e)

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
  either (liftIO . E.throwIO) return $ readerFor (pageFormat page') (pageLHS page') (pageText page')

-- | Detects if the page is a redirect page and handles accordingly. The exact
-- behaviour is as follows:
--
-- If the page is /not/ a redirect page (the most common case), then check the
-- referer to see if the client came to this page as a result of a redirect
-- from another page. If so, then add a notice to the messages to notify the
-- user that they were redirected from another page, and provide a link back
-- to the original page, with an extra parameter to disable redirection
-- (e.g., to allow the original page to be edited).
--
-- If the page /is/ a redirect page, then check the query string for the
-- @redirect@ parameter. This can modify the behaviour of the redirect as
-- follows:
--
-- 1. If the @redirect@ parameter is unset, then check the referer to see if
--    client came to this page as a result of a redirect from another page. If
--    so, then do not redirect, and add a notice to the messages explaining
--    that this page is a redirect page, that would have redirected to the
--    destination given in the metadata (and provide a link thereto), but this
--    was stopped because a double-redirect was detected. This is a simple way
--    to prevent cyclical redirects and other abuses enabled by redirects.
--    redirect to the same page. If the client did /not/ come to this page as
--    a result of a redirect, then redirect back to the same page, except with
--    the redirect parameter set to @\"yes\"@.
--
-- 2. If the @redirect@ parameter is set to \"yes\", then redirect to the
--    destination specificed in the metadata. This uses a client-side (meta
--    refresh + javascript backup) redirect to make sure the referer is set to
--    this URL.
--
-- 3. If the @redirect@ parameter is set to \"no\", then do not redirect, but
--    add a notice to the messages that this page /would/ have redirected to
--    the destination given in the metadata had it not been disabled, and
--    provide a link to the destination given in the metadata. This behaviour
--    is the @revision@ parameter is present in the query string.
handleRedirects :: Page -> ContentTransformer (Either Response Page)
handleRedirects page = case lookup "redirect" (pageMeta page) of
    Nothing -> isn'tRedirect
    Just destination -> isRedirect destination
  where
    addMessage message = modifyContext $ \context -> context
        { ctxLayout = (ctxLayout context)
            { pgMessages = pgMessages (ctxLayout context) ++ [message]
            }
        }
    redirectedFrom source = do
        (url, html) <- processSource source
        return $ concat
            [ "Redirected from <a href=\""
            , url
            , "?redirect=no\" title=\"Go to original page\">"
            , html
            , "</a>"
            ]
    doubleRedirect source destination = do
        (url, html) <- processSource source
        (url', html') <- processDestination destination
        return $ concat
            [ "This page normally redirects to <a href=\""
            , url'
            , "\" title=\"Continue to destination\">"
            , html'
            , "</a>, but as you were already redirected from <a href=\""
            , url
            , "?redirect=no\" title=\"Go to original page\">"
            , html
            , "</a>"
            , ", this was stopped to prevent a double-redirect."
            ]
    cancelledRedirect destination = do
        (url', html') <- processDestination destination
        return $ concat
            [ "This page redirects to <a href=\""
            , url'
            , "\" title=\"Continue to destination\">"
            , html'
            , "</a>."
            ]
    processSource source = do
        base' <- getWikiBase
        let url = stringToHtmlString $ base' ++ urlForPage source
        let html = stringToHtmlString source
        return (url, html)
    processDestination destination = do
        base' <- getWikiBase
        let (page', fragment) = break (== '#') destination
        let url = stringToHtmlString $ concat
             [ base'
             , urlForPage page'
             , fragment
             ]
        let html = stringToHtmlString page'
        return (url, html)
    getSource = do
        cfg <- lift getConfig
        base' <- getWikiBase
        request <- askRq
        return $ do
            referer <- getHeader "referer" request
            uri <- case parseURI laxURIParserOptions referer of
                Left _ -> Nothing
                Right uri -> Just uri
            let Query params = uriQuery uri
            redirect' <- lookup (SC.pack "redirect") params
            guard $ redirect' == SC.pack "yes"
            path' <- stripPrefix (base' ++ "/") (SC.unpack (uriPath uri))
            let path'' = if null path' then frontPage cfg else urlDecode path'
            guard $ isPage path''
            return path''
    withBody = setContentType "text/html; charset=utf-8" . toResponse
    isn'tRedirect = do
        getSource >>= traverse_ (redirectedFrom >=> addMessage)
        return (Right page)
    isRedirect destination = do
        params <- getParams
        case maybe (pRedirect params) (\_ -> Just False) (pRevision params) of
             Nothing -> do
                source <- getSource
                case source of
                     Just source' -> do
                        doubleRedirect source' destination >>= addMessage
                        return (Right page)
                     Nothing -> fmap Left $ do
                        base' <- getWikiBase
                        let url' = concat
                             [ base'
                             , urlForPage (pageName page)
                             , "?redirect=yes"
                             ]
                        lift $ seeOther url' $ withBody $ concat
                            [ "<!doctype html><html><head><title>307 Redirect"
                            , "</title></head><body><p>You are being <a href=\""
                            , stringToHtmlString url'
                            , "\">redirected</a>.</body></p></html>"
                            ]
             Just True -> fmap Left $ do
                (url', html') <- processDestination destination
                lift $ ok $ withBody $ concat
                    [ "<!doctype html><html><head><title>Redirecting to "
                    , html'
                    , "</title><meta http-equiv=\"refresh\" contents=\"0; url="
                    , url'
                    , "\" /><script type=\"text/javascript\">window.location=\""
                    , url'
                    , "\"</script></head><body><p>Redirecting to <a href=\""
                    , url'
                    , "\">"
                    , html'
                    , "</a>...</p></body></html>"
                    ]
             Just False -> do
                cancelledRedirect destination >>= addMessage
                return (Right page)

-- | Converts contents of page file to Page object.
contentsToPage :: String -> ContentTransformer Page
contentsToPage s = do
  cfg <- lift getConfig
  pn <- getPageName
  return $ stringToPage cfg pn s

-- | Converts pandoc document to HTML.
pandocToHtml :: Pandoc -> ContentTransformer Html
pandocToHtml pandocContents = do
  toc <- liftM ctxTOC get
  bird <- liftM ctxBirdTracks get
  cfg <- lift getConfig
  let tpl = "$if(toc)$<div id=\"TOC\">\n$toc$\n</div>\n$endif$\n$body$"
  compiledTemplate <- liftIO $ runIOorExplode $ do
    res <- runWithDefaultPartials $ compileTemplate "toc" tpl
    case res of
      Right t -> return t
      Left e  -> throwError $ PandocTemplateError $ T.pack e
  return $ primHtml $ T.unpack .
           (if xssSanitize cfg then sanitizeBalance else id) $
           either E.throw id . runPure $ writeHtml5String def{
                        writerTemplate = Just compiledTemplate
                      , writerHTMLMathMethod =
                            case mathMethod cfg of
                                 MathML -> Pandoc.MathML
                                 WebTeX u -> Pandoc.WebTeX $ T.pack u
                                 MathJax u -> Pandoc.MathJax $ T.pack u
                                 RawTeX -> Pandoc.PlainMath
                      , writerTableOfContents = toc
                      , writerHighlightStyle = Just pygments
                      , writerExtensions = if bird
                                              then enableExtension Ext_literate_haskell
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
  case syntaxesByFilename defaultSyntaxMap file of
        []    -> mzero
        (l:_) -> case tokenize TokenizerConfig{
                              syntaxMap = defaultSyntaxMap
                            , traceOutput = False} l
                        $ T.pack $ filter (/='\r') source of
                    Left e ->  fail (show e)
                    Right r -> return $ primHtml $ Blaze.renderHtml
                                      $ formatHtmlBlock formatOpts r

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
              else Pandoc
                    (B.setMeta "title" (B.str (T.pack title')) nullMeta)
                    blocks

-- | Adds javascript links for math support.
addMathSupport :: a -> ContentTransformer a
addMathSupport c = do
  conf <- lift getConfig
  updateLayout $ \l ->
    case mathMethod conf of
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

readerFor :: PageType -> Bool -> String -> Either PandocError Pandoc
readerFor pt lhs =
  let defExts = getDefaultExtensions $ T.toLower $ T.pack $ show pt
      defPS = def{ readerExtensions = defExts
                                      <> extensionsFromList [Ext_emoji]
                                      <> getPageTypeDefaultExtensions pt lhs
                                      <> readerExtensions def }
  in runPure . (case pt of
       RST        -> readRST defPS
       Markdown   -> readMarkdown defPS
       CommonMark -> readCommonMark defPS
       LaTeX      -> readLaTeX defPS
       HTML       -> readHtml defPS
       Textile    -> readTextile defPS
       Org        -> readOrg defPS
       DocBook    -> readDocBook defPS
       MediaWiki  -> readMediaWiki defPS) . T.pack

wikiLinksTransform :: Pandoc -> PluginM Pandoc
wikiLinksTransform pandoc
  = do cfg <- liftM pluginConfig ask -- Can't use askConfig from Interface due to circular dependencies.
       return (bottomUp (convertWikiLinks cfg) pandoc)

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Config -> Inline -> Inline
convertWikiLinks cfg (Link attr ref ("", "")) | useAbsoluteUrls cfg =
  Link attr ref (T.pack ("/" </> baseUrl cfg </> inlinesToURL ref),
                 "Go to wiki page")
convertWikiLinks _cfg (Link attr ref ("", "")) =
  Link attr ref (T.pack (inlinesToURL ref), "Go to wiki page")
convertWikiLinks _cfg x = x

-- | Derives a URL from a list of Pandoc Inline elements.
inlinesToURL :: [Inline] -> String
inlinesToURL = encString False isUnescapedInURI . inlinesToString

-- | Convert a list of inlines into a string.
inlinesToString :: [Inline] -> String
inlinesToString = T.unpack . mconcat . map go
  where go :: Inline -> T.Text
        go x = case x of
               Str s                   -> s
               Emph xs                 -> mconcat $ map go xs
               Strong xs               -> mconcat $ map go xs
               Strikeout xs            -> mconcat $ map go xs
               Superscript xs          -> mconcat $ map go xs
               Subscript xs            -> mconcat $ map go xs
               SmallCaps xs            -> mconcat $ map go xs
               Quoted DoubleQuote xs   -> "\"" <> mconcat (map go xs) <> "\""
               Quoted SingleQuote xs   -> "'" <> mconcat (map go xs) <> "'"
               Cite _ xs               -> mconcat $ map go xs
               Code _ s                -> s
               Space                   -> " "
               SoftBreak               -> " "
               LineBreak               -> " "
               Math DisplayMath s      -> "$$" <> s <> "$$"
               Math InlineMath s       -> "$" <> s <> "$"
               RawInline (Format "tex") s -> s
               RawInline _ _           -> ""
               Link _ xs _             -> mconcat $ map go xs
               Image _ xs _            -> mconcat $ map go xs
               Note _                  -> ""
               Span _ xs               -> mconcat $ map go xs

