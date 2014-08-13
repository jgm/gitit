{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>

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

{- Functions for exporting wiki pages in various formats.
-}

module Network.Gitit.Export ( exportFormats ) where
import Text.Pandoc hiding (HTMLMathMethod(..))
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained as SelfContained
import Text.Pandoc.Shared (readDataFileUTF8)
import Network.Gitit.Server
import Network.Gitit.Framework (pathForPage, getWikiBase)
import Network.Gitit.State (getConfig)
import Network.Gitit.Types
import Network.Gitit.Cache (cacheContents, lookupCache)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)
import Text.XHtml (noHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import System.FilePath ((</>), takeDirectory)
import Control.Exception (throwIO)
import System.Directory (doesFileExist)
import Text.HTML.SanitizeXSS
import Text.Pandoc.Writers.RTF (writeRTFWithEmbeddedImages)
import qualified Data.Text as T
import Data.List (isPrefixOf)
import Text.Highlighting.Kate (styleToCss, pygments)
import Paths_gitit (getDataFileName)

defaultRespOptions :: WriterOptions
defaultRespOptions = def { writerStandalone = True, writerHighlight = True }

respond :: String
        -> String
        -> (Pandoc -> IO L.ByteString)
        -> String
        -> Pandoc
        -> Handler
respond mimetype ext fn page doc = liftIO (fn doc) >>=
  ok . setContentType mimetype .
  (if null ext then id else setFilename (page ++ "." ++ ext)) .
  toResponseBS B.empty

respondX :: String -> String -> String
          -> (WriterOptions -> Pandoc -> IO L.ByteString)
          -> WriterOptions -> String -> Pandoc -> Handler
respondX templ mimetype ext fn opts page doc = do
  cfg <- getConfig
  template' <- liftIO $ getDefaultTemplate (pandocUserData cfg) templ
  template <- case template' of
                  Right t  -> return t
                  Left e   -> liftIO $ throwIO e
  doc' <- if ext `elem` ["odt","pdf","beamer","epub","docx","rtf"]
             then fixURLs page doc
             else return doc
  respond mimetype ext (fn opts{writerTemplate = template
                               ,writerUserDataDir = pandocUserData cfg})
          page doc'

respondS :: String -> String -> String -> (WriterOptions -> Pandoc -> String)
          -> WriterOptions -> String -> Pandoc -> Handler
respondS templ mimetype ext fn =
  respondX templ mimetype ext (\o d -> return $ fromString $ fn o d)

respondSlides :: String -> HTMLSlideVariant -> String -> Pandoc -> Handler
respondSlides templ slideVariant page doc = do
    cfg <- getConfig
    base' <- getWikiBase
    let math = case mathMethod cfg of
                   MathML       -> Pandoc.MathML Nothing
                   WebTeX u     -> Pandoc.WebTeX u
                   JsMathScript -> Pandoc.JsMath
                                    (Just $ base' ++ "/js/jsMath/easy/load.js")
                   _            -> Pandoc.PlainMath
    let opts' = defaultRespOptions {
                     writerSlideVariant = slideVariant
                    ,writerIncremental = True
                    ,writerHtml5 = templ == "dzslides"
                    ,writerHTMLMathMethod = math}
    -- We sanitize the body only, to protect against XSS attacks.
    -- (Sanitizing the whole HTML page would strip out javascript
    -- needed for the slides.)  We then pass the body into the
    -- slide template using the 'body' variable.
    Pandoc meta blocks <- fixURLs page doc
    let body' = writeHtmlString opts'{writerStandalone = False}
                   (Pandoc meta blocks) -- just body
    let body'' = T.unpack
               $ (if xssSanitize cfg then sanitizeBalance else id)
               $ T.pack body'
    variables' <- if mathMethod cfg == MathML
                     then do
                        s <- liftIO $ readDataFileUTF8 (pandocUserData cfg)
                                  "MathMLinHTML.js"
                        return [("mathml-script", s)]
                     else return []
    template' <- liftIO $ getDefaultTemplate (pandocUserData cfg) templ
    template <- case template' of
                     Right t  -> return t
                     Left e   -> liftIO $ throwIO e
    dzcore <- if templ == "dzslides"
                  then do
                    dztempl <- liftIO $ readDataFileUTF8 (pandocUserData cfg)
                           $ "dzslides" </> "template.html"
                    return $ unlines
                        $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                        $ lines dztempl
                  else return ""
    let opts'' = opts'{
                writerVariables =
                  ("body",body''):("dzslides-core",dzcore):("highlighting-css",pygmentsCss):variables'
               ,writerTemplate = template
               ,writerUserDataDir = pandocUserData cfg
               }
    let h = writeHtmlString opts'' (Pandoc meta [])
#if MIN_VERSION_pandoc(1,13,0)
    h' <- liftIO $ makeSelfContained opts'' h
#else
    h' <- liftIO $ makeSelfContained (pandocUserData cfg) h
#endif
    ok . setContentType "text/html;charset=UTF-8" .
      -- (setFilename (page ++ ".html")) .
      toResponseBS B.empty $ fromString h'

respondLaTeX :: String -> Pandoc -> Handler
respondLaTeX = respondS "latex" "application/x-latex" "tex"
  writeLaTeX defaultRespOptions

respondConTeXt :: String -> Pandoc -> Handler
respondConTeXt = respondS "context" "application/x-context" "tex"
  writeConTeXt defaultRespOptions


respondRTF :: String -> Pandoc -> Handler
respondRTF = respondX "rtf" "application/rtf" "rtf"
  (\o d -> fromString `fmap` writeRTFWithEmbeddedImages o d) defaultRespOptions

respondRST :: String -> Pandoc -> Handler
respondRST = respondS "rst" "text/plain; charset=utf-8" ""
  writeRST defaultRespOptions{writerReferenceLinks = True}

respondMarkdown :: String -> Pandoc -> Handler
respondMarkdown = respondS "markdown" "text/plain; charset=utf-8" ""
  writeMarkdown defaultRespOptions{writerReferenceLinks = True}

respondPlain :: String -> Pandoc -> Handler
respondPlain = respondS "plain" "text/plain; charset=utf-8" ""
  writePlain defaultRespOptions

respondMan :: String -> Pandoc -> Handler
respondMan = respondS "man" "text/plain; charset=utf-8" ""
  writeMan defaultRespOptions

respondTexinfo :: String -> Pandoc -> Handler
respondTexinfo = respondS "texinfo" "application/x-texinfo" "texi"
  writeTexinfo defaultRespOptions

respondDocbook :: String -> Pandoc -> Handler
respondDocbook = respondS "docbook" "application/docbook+xml" "xml"
  writeDocbook defaultRespOptions

respondOrg :: String -> Pandoc -> Handler
respondOrg = respondS "org" "text/plain; charset=utf-8" ""
  writeOrg defaultRespOptions

respondICML :: String -> Pandoc -> Handler
respondICML = respondS "icml" "application/xml; charset=utf-8" ""
  writeICML defaultRespOptions

respondTextile :: String -> Pandoc -> Handler
respondTextile = respondS "textile" "text/plain; charset=utf-8" ""
  writeTextile defaultRespOptions

respondAsciiDoc :: String -> Pandoc -> Handler
respondAsciiDoc = respondS "asciidoc" "text/plain; charset=utf-8" ""
  writeAsciiDoc defaultRespOptions

respondMediaWiki :: String -> Pandoc -> Handler
respondMediaWiki = respondS "mediawiki" "text/plain; charset=utf-8" ""
  writeMediaWiki defaultRespOptions

respondODT :: String -> Pandoc -> Handler
respondODT = respondX "opendocument" "application/vnd.oasis.opendocument.text"
              "odt" writeODT defaultRespOptions

respondEPUB :: String -> Pandoc -> Handler
respondEPUB = respondX "html" "application/epub+zip" "epub" writeEPUB
               defaultRespOptions

respondDocx :: String -> Pandoc -> Handler
respondDocx = respondX "native"
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  "docx" writeDocx defaultRespOptions

respondPDF :: Bool -> String -> Pandoc -> Handler
respondPDF useBeamer page old_pndc = fixURLs page old_pndc >>= \pndc -> do
  cfg <- getConfig
  unless (pdfExport cfg) $ error "PDF export disabled"
  let cacheName = pathForPage page ++ ".export.pdf"
  cached <- if useCache cfg
               then lookupCache cacheName
               else return Nothing
  pdf' <- case cached of
            Just (_modtime, bs) -> return $ Right $ L.fromChunks [bs]
            Nothing -> do
              template' <- liftIO $ getDefaultTemplate (pandocUserData cfg)
                                  $ if useBeamer then "beamer" else "latex"
              template  <- liftIO $ either throwIO return template'
              let toc = tableOfContents cfg
              res <- liftIO $ makePDF "pdflatex" writeLaTeX
                         defaultRespOptions{writerTemplate = template
                                           ,writerSourceURL = Just $ baseUrl cfg
                                           ,writerTableOfContents = toc
                                           ,writerBeamer = useBeamer} pndc
              return res
  case pdf' of
       Left logOutput -> simpleErrorHandler ("PDF creation failed:\n"
                           ++ toString logOutput)
       Right pdfBS -> do
              case cached of
                Nothing ->
                     cacheContents cacheName $ B.concat . L.toChunks $ pdfBS
                _ -> return ()
              ok $ setContentType "application/pdf" $ setFilename (page ++ ".pdf") $
                        (toResponse noHtml) {rsBody = pdfBS}

-- | When we create a PDF or ODT from a Gitit page, we need to fix the URLs of any
-- images on the page. Those URLs will often be relative to the staticDir, but the
-- PDF or ODT processor only understands paths relative to the working directory.
--
-- Because the working directory will not in general be the root of the gitit instance
-- at the time the Pandoc is fed to e.g. pdflatex, this function replaces the URLs of
-- images in the staticDir with their correct absolute file path.
fixURLs :: String -> Pandoc -> GititServerPart Pandoc
fixURLs page pndc = do
    cfg <- getConfig
    defaultStatic <- liftIO $ getDataFileName $ "data" </> "static"

    let static = staticDir cfg
    let repoPath = repositoryPath cfg

    let go (Image ils (url, title)) = do
           fixedURL <- fixURL url
           return $ Image ils (fixedURL, title)
        go x                        = return x

        fixURL ('/':url) = resolve url
        fixURL url       = resolve $ takeDirectory page </> url

        resolve p = do
           sp <- doesFileExist $ static </> p
           dsp <- doesFileExist $ defaultStatic </> p
           return (if sp then static </> p
                   else (if dsp then defaultStatic </> p
                         else repoPath </> p))
    liftIO $ bottomUpM go pndc

exportFormats :: Config -> [(String, String -> Pandoc -> Handler)]
exportFormats cfg = if pdfExport cfg
                       then ("PDF", respondPDF False) :
                            ("Beamer", respondPDF True) :
                            rest
                       else rest
   where rest = [ ("LaTeX",     respondLaTeX)     -- (description, writer)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("Markdown",  respondMarkdown)
                , ("Plain text",respondPlain)
                , ("MediaWiki", respondMediaWiki)
                , ("Org-mode",  respondOrg)
                , ("ICML",      respondICML)
                , ("Textile",   respondTextile)
                , ("AsciiDoc",  respondAsciiDoc)
                , ("Man page",  respondMan)
                , ("DocBook",   respondDocbook)
                , ("DZSlides",  respondSlides "dzslides" DZSlides)
                , ("Slidy",     respondSlides "slidy" SlidySlides)
                , ("S5",        respondSlides "s5" S5Slides)
                , ("EPUB",      respondEPUB)
                , ("ODT",       respondODT)
                , ("DOCX",      respondDocx)
                , ("RTF",       respondRTF) ]

pygmentsCss :: String
pygmentsCss = styleToCss pygments
