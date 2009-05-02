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

module Gitit.ContentTransformer
  (
  -- ContentTransformer runners
    runPageTransformer
  , runFileTransformer
  -- Gitit responders
  , showRawPage
  , showFileAsText
  , showPage
  , exportPage
  , showHighlightedSource
  , showFile
  , preview
  -- Cache-aware transformer combinators
  , mbTextToWikiPandocPageCached
  , pandocToWikiDivCached
  , highlightSourceCached
  -- Cache support for transformers
  , skipIfCached
  , useCache
  , cacheHtml
  -- Content retrieval combinators
  , rawContents
  , cachedContents
  -- Response-generating combinators
  , textResponse
  , mimeFileResponse
  , mimeResponse
  , exportPandoc
  , applyWikiTemplate
  , htmlResponse
  , utf8Response
  -- Content-type transformation combinators
  , mbTextToWikiPandocPage
  , textToWikiPandocPage
  , textToWikiPandoc
  , textToPandoc
  , maybePandocToHtml
  , pandocToHtml
  , highlightSource
  , pandocToWikiDiv
  -- Content or context augmentation combinators
  , applyPageTransforms
  , wikiDivify
  , addPageNameToPandoc
  , addMathSupport
  , addScripts
  -- ContentTransformer context API
  , getPageName
  , getFileName
  , getPageType
  , getLayout
  , getParams
  , getCacheable
  -- Pandoc and wiki content conversion support
  , inlinesToURL
  , inlinesToString
  )
where

import Prelude hiding (catch)
import Gitit.Server
import Gitit.Framework
import Gitit.State
import Gitit.Types
import Gitit.Layout
import Gitit.Export (exportFormats)
import qualified Data.FileStore as FS
import Data.Maybe (mapMaybe)
import Text.Pandoc
import Text.Pandoc.Shared (HTMLMathMethod(..))
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import Text.Highlighting.Kate
import Data.Maybe (isNothing)
import Codec.Binary.UTF8.String (encodeString)
import System.FilePath
import Control.Monad.State
import Control.Exception (throwIO, catch)
import Network.HTTP (urlEncodeVars)
import Happstack.Server (WebT, ToMessage)
import Network.URI (isAllowedInURI, escapeURIString)

type ContentTransformer = StateT Context (WebT IO)  -- Web a = WebT IO a

--
-- ContentTransformer runners
--

runPageTransformer :: ContentTransformer Response
                   -> String
                   -> Params
                   -> Web Response
runPageTransformer xform page params = do
  pt <- getDefaultPageType
  evalStateT xform $
    Context page (pathForPage page) pt defaultPageLayout params True

runFileTransformer :: ContentTransformer Response
                   -> FilePath
                   -> Params
                   -> Web Response
runFileTransformer xform file params = do
  pt <- getDefaultPageType
  evalStateT xform $
    Context file file pt defaultPageLayout params True

--
-- Gitit responders
--

showRawPage :: String -> Params -> Web Response
showRawPage = runPageTransformer rawTextResponse

showFileAsText :: FilePath -> Params -> Web Response
showFileAsText = runFileTransformer rawTextResponse

showPage :: String -> Params -> Web Response
showPage = runPageTransformer htmlViaPandoc

exportPage :: String -> Params -> Web Response
exportPage = runPageTransformer exportViaPandoc

showHighlightedSource :: FilePath -> Params -> Web Response
showHighlightedSource = runFileTransformer highlightRawSource

showFile :: FilePath -> Params -> Web Response
showFile = runFileTransformer (rawContents >>= mimeFileResponse)

preview :: String -> Params -> Web Response
preview = runPageTransformer $
          getParams >>=
          textToWikiPandoc . pRaw >>=
          pandocToHtml >>=
          utf8Response . renderHtmlFragment

--
-- Top level, composed transformers
--

rawTextResponse :: ContentTransformer Response
rawTextResponse = rawContents >>= textResponse

exportViaPandoc :: ContentTransformer Response
exportViaPandoc = rawContents >>= mbTextToWikiPandocPage >>= exportPandoc

htmlViaPandoc :: ContentTransformer Response
htmlViaPandoc = cachedContents >>=
                mbTextToWikiPandocPageCached >>=
                pandocToWikiDivCached >>=
                addMathSupport >>=
                applyWikiTemplate

highlightRawSource :: ContentTransformer Response
highlightRawSource = do
    updateLayout $ \l -> l { pgTabs = [ViewTab,HistoryTab] }
    cachedContents >>= highlightSourceCached >>= applyWikiTemplate

--
-- Cache-aware transformer combinators
--

mbTextToWikiPandocPageCached :: Either (Maybe String) Html
                             -> ContentTransformer (Either (Maybe Pandoc) Html)
mbTextToWikiPandocPageCached = skipIfCached mbTextToWikiPandocPage

pandocToWikiDivCached :: Either (Maybe Pandoc) Html -> ContentTransformer Html
pandocToWikiDivCached = useCache pandocToWikiDiv

highlightSourceCached :: Either (Maybe String) Html -> ContentTransformer Html
highlightSourceCached = useCache highlightSource

--
-- Cache support for transformers
--

-- | Returns a cache-aware version of the provided transformer.  The returned
-- transformer, when applied to cached content represented as (Right c),
-- returns the cache unchanged.  The provided transformer is only evaluated
-- when uncached content is provided, represented as (Left x).
skipIfCached :: (Monad m) => (a -> m b) -> Either a c -> m (Either b c)
skipIfCached f (Left x)  = liftM Left (f x)
skipIfCached _ (Right c) = return (Right c)

-- | Returns a cache-enabled version of the provided transformer.  The returned
-- transformer either returns the contents of the cache, if any, or applies the
-- provided transformer to the uncached content, and caches and returns the
-- result.
useCache :: (a -> ContentTransformer Html)
         -> Either a Html
         -> ContentTransformer Html
useCache f (Left c)  = f c >>= cacheHtml
useCache _ (Right c) = return c

cacheHtml :: Html -> ContentTransformer Html
cacheHtml c = do
  params <- getParams
  file <- getFileName
  cacheable <- getCacheable
  when (isNothing (pRevision params) && cacheable) $ do
    -- TODO not ideal, since page might have been modified
    -- after being retrieved by pageAsPandoc...
    -- better to have pageAsPandoc return the revision ID too...
    fs <- getFileStore
    rev <- liftIO $ FS.latest fs file
    cacheContents file rev c
  return c

--
-- Content retrieval combinators
--

-- | Returns raw file contents
rawContents :: ContentTransformer (Maybe String)
rawContents = do
  params <- getParams
  file <- getFileName
  fs <- getFileStore
  let rev = pRevision params
  liftIO $ catch (liftM Just $ FS.retrieve fs file rev)
                 (\e -> if e == FS.NotFound then return Nothing else throwIO e)

-- | Returns cached page if available, otherwise raw file contents
cachedContents :: ContentTransformer (Either (Maybe String) Html)
cachedContents = do
  file <- getFileName
  params <- getParams
  cp <- lookupCache file (pRevision params)
  maybe (liftM Left rawContents) (return . Right) cp

--
-- Response-generating combinators
--

textResponse :: Maybe String -> ContentTransformer Response
textResponse Nothing  = mzero  -- fail quietly if file not found
textResponse (Just c) = mimeResponse c "text/plain; charset=utf-8"

mimeFileResponse :: Maybe String -> ContentTransformer Response
mimeFileResponse Nothing = error "Unable to retrieve file contents."
mimeFileResponse (Just c) =
  mimeResponse c =<< getMimeTypeForExtension . takeExtension =<< getFileName

mimeResponse :: Monad m => String -> String -> m Response
mimeResponse c mimeType =
  return . setContentType mimeType . toResponse . encodeString $ c

-- | Exports Pandoc as Response using format specified in Params
exportPandoc :: Maybe Pandoc -> ContentTransformer Response
exportPandoc Nothing = error "Unable to retrieve page contents."
exportPandoc (Just doc) = do
  params <- getParams
  page <- getPageName
  let format = pFormat params
  case lookup format exportFormats of
       Nothing     -> error $ "Unknown export format: " ++ format
       Just writer -> lift (writer page doc)

applyWikiTemplate :: Html -> ContentTransformer Response
applyWikiTemplate c = do
  Context { ctxLayout = layout, ctxPage = page, ctxParams = params } <- get
  lift $ formattedPage layout page params c

-- | Returns specified content as a Response
htmlResponse :: (Monad m, ToMessage a) => a -> m Response
htmlResponse = return . toResponse

-- | Encode string as UTF8 and return as Response
utf8Response :: String -> ContentTransformer Response
utf8Response = return . toResponse . encodeString

--
-- Content-type transformation combinators
--

-- | Same as textToWikiPandocPage, with support for Maybe values
mbTextToWikiPandocPage :: Maybe String -> ContentTransformer (Maybe Pandoc)
mbTextToWikiPandocPage Nothing  = mzero
mbTextToWikiPandocPage (Just c) = return . Just =<< textToWikiPandocPage c

-- | Converts source text to Pandoc, applies page transforms, and adds page
-- name to Pandoc meta info
textToWikiPandocPage :: String -> ContentTransformer Pandoc
textToWikiPandocPage = textToWikiPandoc >=> addPageNameToPandoc

-- | Converts source text to Pandoc and applies page transforms
textToWikiPandoc :: String -> ContentTransformer Pandoc
textToWikiPandoc = textToPandoc >=> applyPageTransforms

-- | Converts source text to Pandoc using default page type
textToPandoc :: String -> ContentTransformer Pandoc
textToPandoc s = do
  pt <- getPageType -- should get the current page type instead
  return $ readerFor pt $ filter (/= '\r') s

-- | Same as pandocToHtml, with support for Maybe values
maybePandocToHtml :: (MonadPlus m, MonadIO m) => Maybe Pandoc -> m Html
maybePandocToHtml = maybe mzero pandocToHtml

-- | Converts pandoc document to HTML.
pandocToHtml :: MonadIO m => Pandoc -> m Html
pandocToHtml pandocContents = do
  cfg <- getConfig
  return $ writeHtml defaultWriterOptions{
                        writerStandalone = False
                      , writerHTMLMathMethod = JsMath
                               (Just "/_static/js/jsMath/easy/load.js")
                      , writerTableOfContents = tableOfContents cfg
                      } pandocContents

highlightSource :: Maybe String -> ContentTransformer Html
highlightSource Nothing = mzero
highlightSource (Just source) = do
  file <- getFileName
  let lang' = head $ languagesByExtension $ takeExtension file
  case highlightAs lang' (filter (/='\r') source) of
       Left _       -> mzero
       Right res    -> return $ formatAsXHtml [OptNumberLines] lang' $! res

pandocToWikiDiv :: Maybe Pandoc -> ContentTransformer Html
pandocToWikiDiv = maybePandocToHtml >=> wikiDivify

--
-- Content or context augmentation combinators
--

getPageTransforms :: ContentTransformer [Pandoc -> PluginM Pandoc]
getPageTransforms = liftM (mapMaybe pageTransform) $ queryAppState plugins
  where pageTransform (PageTransform x) = Just x
        pageTransform _                 = Nothing

applyPageTransforms :: Pandoc -> ContentTransformer Pandoc
applyPageTransforms c = do
  context <- get
  conf <- lift getConfig
  transforms <- getPageTransforms
  let combined = foldM (flip ($)) c (wikiLinksTransform:transforms)
  (result, context') <- liftIO $ runPluginM combined conf context
  put context'
  return result

wikiDivify :: Html -> ContentTransformer Html
wikiDivify c = do
  params <- getParams
  let dblClickJs = "window.location = window.location + '?edit" ++
                   case pRevision params of
                        Nothing   -> "';"
                        Just r    -> ("&" ++ urlEncodeVars [("revision", r),
                              ("logMsg", "Revert to " ++ r)] ++ "';")
  return $ thediv ! [identifier "wikipage",
                     strAttr "onDblClick" dblClickJs] << c

addPageNameToPandoc :: Pandoc -> ContentTransformer Pandoc
addPageNameToPandoc (Pandoc _ blocks) = do
  page <- getPageName
  return $ Pandoc (Meta [Str page] [] []) blocks

addMathSupport :: a -> ContentTransformer a
addMathSupport c = do
  jsMathExists <- queryAppState jsMath
  updateLayout $ \l -> addScripts l ["jsMath/easy/load.js" | jsMathExists]
  return c

addScripts :: PageLayout -> [String] -> PageLayout
addScripts layout scriptPaths =
  layout{ pgScripts = scriptPaths ++ pgScripts layout }

--
-- ContentTransformer context API
--

getParams :: ContentTransformer Params
getParams = liftM ctxParams get

getPageName :: ContentTransformer String
getPageName = liftM ctxPage get

getPageType :: ContentTransformer PageType
getPageType = liftM ctxPageType get

getFileName :: ContentTransformer FilePath
getFileName = liftM ctxFile get

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

readerFor :: PageType -> (String -> Pandoc)
readerFor pt = case pt of
                 RST      -> readRST defaultParserState{
                                       stateSanitizeHTML = True,
                                       stateSmart = True
                                       }
                 Markdown -> readMarkdown defaultParserState{
                                            stateSanitizeHTML = True,
                                            stateSmart = True
                                            }

wikiLinksTransform :: Pandoc -> PluginM Pandoc
wikiLinksTransform = return . processWith convertWikiLinks

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Inline -> Inline
convertWikiLinks (Link ref ("", "")) =
  Link ref (inlinesToURL ref, "Go to wiki page")
convertWikiLinks x = x

inlinesToURL :: [Inline] -> String
inlinesToURL = escapeURIString isAllowedInURI  . encodeString . inlinesToString

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
               Code s                  -> s
               Space                   -> " "
               EmDash                  -> "---"
               EnDash                  -> "--"
               Apostrophe              -> "'"
               Ellipses                -> "..."
               LineBreak               -> " "
               Math DisplayMath s      -> "$$" ++ s ++ "$$"
               Math InlineMath s       -> "$" ++ s ++ "$"
               TeX s                   -> s
               HtmlInline _            -> ""
               Link xs _               -> concatMap go xs
               Image xs _              -> concatMap go xs
               Note _                  -> ""

