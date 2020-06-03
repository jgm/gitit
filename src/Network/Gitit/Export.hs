{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Exception (throwIO)
import Text.Pandoc hiding (HTMLMathMethod(..), getDataFileName)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.SelfContained as SelfContained
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Map as M
import Network.Gitit.Server
import Network.Gitit.Framework (pathForPage)
import Network.Gitit.State (getConfig)
import Network.Gitit.Types
import Network.Gitit.Cache (cacheContents, lookupCache)
import Text.DocTemplates as DT
import Control.Monad.Trans (liftIO)
import Control.Monad (unless)
import Text.XHtml (noHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist)
import Text.HTML.SanitizeXSS
import Data.ByteString.Lazy (fromStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.List (isPrefixOf)
import Skylighting (styleToCss, pygments)
import Paths_gitit (getDataFileName)

defaultRespOptions :: WriterOptions
defaultRespOptions = def { writerHighlightStyle = Just pygments }

respondX :: String -> String -> String
          -> (WriterOptions -> Pandoc -> PandocIO L.ByteString)
          -> WriterOptions -> String -> Pandoc -> Handler
respondX templ mimetype ext fn opts page doc = do
  cfg <- getConfig
  doc' <- if ext `elem` ["odt","pdf","beamer","epub","docx","rtf"]
             then fixURLs page doc
             else return doc
  doc'' <- liftIO $ runIO $ do
        setUserDataDir $ pandocUserData cfg
        compiledTemplate <- compileDefaultTemplate (T.pack templ)
        fn opts{ writerTemplate = Just compiledTemplate } doc'
  either (liftIO . throwIO)
         (ok . setContentType mimetype .
           (if null ext then id else setFilename (page ++ "." ++ ext)) .
            toResponseBS B.empty)
         doc''

respondS :: String -> String -> String -> (WriterOptions -> Pandoc -> PandocIO Text)
          -> WriterOptions -> String -> Pandoc -> Handler
respondS templ mimetype ext fn =
  respondX templ mimetype ext (\o d -> fromStrict . encodeUtf8 <$> fn o d)

respondSlides :: String -> (WriterOptions -> Pandoc -> PandocIO Text) -> String -> Pandoc -> Handler
respondSlides templ fn page doc = do
    cfg <- getConfig
    let math = case mathMethod cfg of
                   MathML       -> Pandoc.MathML
                   WebTeX u     -> Pandoc.WebTeX $ T.pack u
                   _            -> Pandoc.PlainMath
    let opts' = defaultRespOptions { writerIncremental = True
                                   , writerHTMLMathMethod = math}
    -- We sanitize the body only, to protect against XSS attacks.
    -- (Sanitizing the whole HTML page would strip out javascript
    -- needed for the slides.)  We then pass the body into the
    -- slide template using the 'body' variable.
    Pandoc meta blocks <- fixURLs page doc
    docOrError <- liftIO $ runIO $ do
          setUserDataDir $ pandocUserData cfg
          body' <- writeHtml5String opts' (Pandoc meta blocks) -- just body
          let body'' = T.unpack
                       $ (if xssSanitize cfg then sanitizeBalance else id)
                       $ body'
          let setVariable key val (DT.Context ctx) =
                DT.Context $ M.insert (T.pack key) (toVal (T.pack val)) ctx
          variables' <- if mathMethod cfg == MathML
                          then do
                              s <- readDataFile "MathMLinHTML.js"
                              return $ setVariable "mathml-script"
                                         (UTF8.toString s) mempty
                          else return mempty
          compiledTemplate <- compileDefaultTemplate (T.pack templ)
          dzcore <- if templ == "dzslides"
                      then do
                        dztempl <- readDataFile $ "dzslides" </> "template.html"
                        return $ unlines
                            $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                            $ lines $ UTF8.toString dztempl
                      else return ""
          let opts'' = opts'{
                             writerVariables =
                               setVariable "body" body'' $
                               setVariable "dzslides-core" dzcore $
                               setVariable "highlighting-css" pygmentsCss
                               $ variables'
                            ,writerTemplate = Just compiledTemplate }
          h <- fn opts'' (Pandoc meta [])
          makeSelfContained h
    either (liftIO . throwIO)
           (ok . setContentType "text/html;charset=UTF-8" .
             (setFilename (page ++ ".html")) .
             toResponseBS B.empty . L.fromStrict . UTF8.fromText)
           docOrError

respondLaTeX :: String -> Pandoc -> Handler
respondLaTeX = respondS "latex" "application/x-latex" "tex"
  writeLaTeX defaultRespOptions

respondConTeXt :: String -> Pandoc -> Handler
respondConTeXt = respondS "context" "application/x-context" "tex"
  writeConTeXt defaultRespOptions


respondRTF :: String -> Pandoc -> Handler
respondRTF = respondX "rtf" "application/rtf" "rtf"
  (\o d -> L.fromStrict . UTF8.fromText <$> writeRTF o d) defaultRespOptions

respondRST :: String -> Pandoc -> Handler
respondRST = respondS "rst" "text/plain; charset=utf-8" ""
  writeRST defaultRespOptions{writerReferenceLinks = True}

respondMarkdown :: String -> Pandoc -> Handler
respondMarkdown = respondS "markdown" "text/plain; charset=utf-8" ""
  writeMarkdown defaultRespOptions{writerReferenceLinks = True}

respondCommonMark :: String -> Pandoc -> Handler
respondCommonMark = respondS "commonmark" "text/plain; charset=utf-8" ""
  writeCommonMark defaultRespOptions{writerReferenceLinks = True}

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
  writeDocbook5 defaultRespOptions

respondOrg :: String -> Pandoc -> Handler
respondOrg = respondS "org" "text/plain; charset=utf-8" ""
  writeOrg defaultRespOptions

respondICML :: String -> Pandoc -> Handler
respondICML = respondX "icml" "application/xml; charset=utf-8" ""
              (\o d -> L.fromStrict . UTF8.fromText <$> writeICML o d)
                         defaultRespOptions

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
respondEPUB = respondX "html" "application/epub+zip" "epub" writeEPUB3
               defaultRespOptions

respondDocx :: String -> Pandoc -> Handler
respondDocx = respondX "native"
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  "docx" writeDocx defaultRespOptions

respondPDF :: Bool -> String -> Pandoc -> Handler
respondPDF useBeamer page old_pndc = fixURLs page old_pndc >>= \pndc -> do
  cfg <- getConfig
  unless (pdfExport cfg) $ error "PDF export disabled"
  let cacheName = pathForPage page (defaultExtension cfg) ++ ".export.pdf"
  cached <- if useCache cfg
               then lookupCache cacheName
               else return Nothing
  pdf' <- case cached of
            Just (_modtime, bs) -> return $ Right $ L.fromChunks [bs]
            Nothing -> do
              let toc = tableOfContents cfg
              res <- liftIO $ runIO $ do
                setUserDataDir $ pandocUserData cfg
                setInputFiles [baseUrl cfg]
                let templ = if useBeamer then "beamer" else "latex"
                compiledTemplate <- compileDefaultTemplate templ
                makePDF "pdflatex" [] (if useBeamer then writeBeamer else writeLaTeX)
                  defaultRespOptions{ writerTemplate = Just compiledTemplate
                                    , writerTableOfContents = toc } pndc
              either (liftIO . throwIO) return res

  case pdf' of
       Left logOutput' -> simpleErrorHandler ("PDF creation failed:\n"
                           ++ UTF8.toStringLazy logOutput')
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

    let go (Image attr ils (url, title)) = do
           fixedURL <- fixURL $ T.unpack url
           return $ Image attr ils (T.pack fixedURL, title)
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
                , ("CommonMark",respondCommonMark)
                , ("Plain text",respondPlain)
                , ("MediaWiki", respondMediaWiki)
                , ("Org-mode",  respondOrg)
                , ("ICML",      respondICML)
                , ("Textile",   respondTextile)
                , ("AsciiDoc",  respondAsciiDoc)
                , ("Man page",  respondMan)
                , ("DocBook",   respondDocbook)
                , ("DZSlides",  respondSlides "dzslides" writeDZSlides)
                , ("Slidy",     respondSlides "slidy" writeSlidy)
                , ("S5",        respondSlides "s5" writeS5)
                , ("EPUB",      respondEPUB)
                , ("ODT",       respondODT)
                , ("DOCX",      respondDocx)
                , ("RTF",       respondRTF) ]

pygmentsCss :: String
pygmentsCss = styleToCss pygments
