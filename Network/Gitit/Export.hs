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
import Text.Pandoc
import Text.Pandoc.Writers.S5 (s5HeaderIncludes)
import Text.Pandoc.Shared (escapeStringUsing)
import Network.Gitit.Server
import Network.Gitit.Framework (pathForPage)
import Network.Gitit.Util (withTempDir, readFileUTF8)
import Network.Gitit.State (getConfig)
import Network.Gitit.Types
import Network.Gitit.Cache (cacheContents, lookupCache)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless, when)
import Text.XHtml (noHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import System.FilePath ((<.>), (</>))
import Control.Exception (throwIO)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.IO (openTempFile)
import System.Directory (getCurrentDirectory, setCurrentDirectory, removeFile)
import System.Process (runProcess, waitForProcess)
import Codec.Binary.UTF8.String (encodeString)

defaultRespOptions :: WriterOptions
defaultRespOptions = defaultWriterOptions { writerStandalone = True }

respond :: String
        -> String
        -> (Pandoc -> IO L.ByteString)
        -> String
        -> Pandoc
        -> Handler
respond mimetype ext fn page doc = liftIO (fn doc) >>=
  ok . setContentType mimetype .
  (if null ext then id else setFilename (page ++ "." ++ ext)) .
  toResponseBS (B.empty)

respondX :: String -> String -> String
          -> (WriterOptions -> Pandoc -> IO L.ByteString)
          -> WriterOptions -> String -> Pandoc -> Handler
respondX templ mimetype ext fn opts page doc = do
  cfg <- getConfig
  template' <- liftIO $ getDefaultTemplate (pandocUserData cfg) templ
  template <- case template' of
                  Right t  -> return t
                  Left e   -> liftIO $ throwIO e
  doc' <- if ext `elem` ["odt","pdf","epub"]
             then fixURLs doc
             else return doc
  respond mimetype ext (fn opts{writerTemplate = template
                               ,writerSourceDirectory = repositoryPath cfg
                               ,writerUserDataDir = pandocUserData cfg})
          page doc'

respondS :: String -> String -> String -> (WriterOptions -> Pandoc -> String)
          -> WriterOptions -> String -> Pandoc -> Handler
respondS templ mimetype ext fn =
  respondX templ mimetype ext (\o d -> return $ fromString $ fn o d)

respondLaTeX :: String -> Pandoc -> Handler
respondLaTeX = respondS "latex" "application/x-latex" "tex"
  writeLaTeX defaultRespOptions

respondConTeXt :: String -> Pandoc -> Handler
respondConTeXt = respondS "context" "application/x-context" "tex"
  writeConTeXt defaultRespOptions


respondRTF :: String -> Pandoc -> Handler
respondRTF = respondS "rtf" "application/rtf" "rtf"
  writeRTF defaultRespOptions

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

respondS5 :: String -> Pandoc -> Handler
respondS5 pg doc = do
  cfg <- getConfig
  inc <- liftIO $ s5HeaderIncludes (pandocUserData cfg)
  respondS "s5" "text/html; charset=utf-8" ""
    writeS5String
    defaultRespOptions{writerSlideVariant = S5Slides
                      ,writerIncremental = True,
                       writerVariables = [("header-includes",inc)]}
    pg doc

respondSlidy :: String -> Pandoc -> Handler
respondSlidy pg doc = do
  respondS "slidy" "text/html; charset=utf-8" ""
    writeHtmlString
    defaultRespOptions{writerSlideVariant = SlidySlides
                      ,writerIncremental = True}
    pg doc

respondTexinfo :: String -> Pandoc -> Handler
respondTexinfo = respondS "texinfo" "application/x-texinfo" "texi"
  writeTexinfo defaultRespOptions

respondDocbook :: String -> Pandoc -> Handler
respondDocbook = respondS "docbook" "application/docbook+xml" "xml"
  writeDocbook defaultRespOptions

respondMediaWiki :: String -> Pandoc -> Handler
respondMediaWiki = respondS "mediawiki" "text/plain; charset=utf-8" ""
  writeMediaWiki defaultRespOptions

respondODT :: String -> Pandoc -> Handler
respondODT = respondX "opendocument" "application/vnd.oasis.opendocument.text"
              "odt" (writeODT Nothing) defaultRespOptions

respondEPUB :: String -> Pandoc -> Handler
respondEPUB = respondX "html" "application/epub+zip" "epub" (writeEPUB Nothing)
               defaultRespOptions

-- | Run shell command and return error status.  Assumes
-- UTF-8 locale. Note that this does not actually go through \/bin\/sh!
runShellCommand :: FilePath                     -- ^ Working directory
                -> Maybe [(String, String)]     -- ^ Environment
                -> String                       -- ^ Command
                -> [String]                     -- ^ Arguments
                -> IO ExitCode
runShellCommand workingDir environment command optionList = do
  (errPath, err) <- openTempFile workingDir "err"
  hProcess <- runProcess (encodeString command) (map encodeString optionList)
               (Just workingDir) environment Nothing (Just err) (Just err)
  status <- waitForProcess hProcess
  removeFile errPath
  return status

respondPDF :: String -> Pandoc -> Handler
respondPDF page old_pndc = fixURLs old_pndc >>= \pndc -> do
  cfg <- getConfig
  unless (pdfExport cfg) $ error "PDF export disabled"
  let cacheName = pathForPage page ++ ".export.pdf"
  cached <- if useCache cfg
               then lookupCache cacheName
               else return Nothing
  pdf' <- case cached of
            Just (_modtime, bs) -> return $ Right (False, L.fromChunks [bs])
            Nothing -> liftIO $ withTempDir "gitit-tmp-pdf" $ \tempdir -> do
              template' <- liftIO $ getDefaultTemplate (pandocUserData cfg) "latex"
              template  <- either throwIO return template'
              let toc = tableOfContents cfg
              let latex = writeLaTeX defaultRespOptions{writerTemplate = template
                                                       ,writerTableOfContents = toc} pndc
              let tempfile = "export" <.> "tex"
              curdir <- getCurrentDirectory
              setCurrentDirectory tempdir
              writeFile tempfile latex
              -- run pdflatex twice to get the references and toc right
              let cmd = "pdflatex"
              oldEnv <- getEnvironment
              let env = Just $ ("TEXINPUTS",".:" ++ 
                               escapeStringUsing [(' ',"\\ "),('"',"\\\"")]
                               (curdir </> repositoryPath cfg) ++ ":") : oldEnv
              let opts = ["-interaction=batchmode", "-no-shell-escape", tempfile]
              _ <- runShellCommand tempdir env cmd opts
              canary <- runShellCommand tempdir env cmd opts
              setCurrentDirectory curdir -- restore original location
              case canary of
                  ExitSuccess   -> do pdfBS <- L.readFile (tempdir </> "export" <.> "pdf")
                                      return $ Right (useCache cfg, pdfBS)
                  ExitFailure n -> do l <- readFileUTF8 (tempdir </> "export" <.> "log")
                                      return $ Left (n, l)
  case pdf' of
       Left (n,logOutput) -> simpleErrorHandler ("PDF creation failed with code: " ++
                               show n ++ "\n" ++ logOutput)
       Right (needsCaching, pdfBS) -> do
              when needsCaching $
                 cacheContents cacheName $ B.concat . L.toChunks $ pdfBS
              ok $ setContentType "application/pdf" $ setFilename (page ++ ".pdf") $
                        (toResponse noHtml) {rsBody = pdfBS}

-- | When we create a PDF or ODT from a Gitit page, we need to fix the URLs of any
-- images on the page. Those URLs will often be relative to the staticDir, but the
-- PDF or ODT processor only understands paths relative to the working directory.
--
-- Because the working directory will not in general be the root of the gitit instance
-- at the time the Pandoc is fed to e.g. pdflatex, this function replaces the URLs of
-- images in the staticDir with their correct absolute file path.
fixURLs :: Pandoc -> GititServerPart Pandoc
fixURLs pndc = do
    curdir <- liftIO getCurrentDirectory
    cfg <- getConfig
    
    let go (Image ils (url, title)) = Image ils (fixURL url, title)
        go x                        = x
        
        fixURL ('/':url) = curdir </> staticDir cfg </> url
        fixURL url       = url
    
    return $ processWith go pndc

exportFormats :: Config -> [(String, String -> Pandoc -> Handler)]
exportFormats cfg = if pdfExport cfg
                       then ("PDF", respondPDF) : rest
                       else rest
   where rest = [ ("LaTeX",     respondLaTeX)     -- (description, writer)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("markdown",  respondMarkdown)
                , ("plain text",respondPlain)
                , ("MediaWiki", respondMediaWiki)
                , ("man",       respondMan)
                , ("DocBook",   respondDocbook)
                , ("Slidy",     respondSlidy)
                , ("S5",        respondS5)
                , ("ODT",       respondODT)
                , ("EPUB",      respondEPUB)
                , ("RTF",       respondRTF) ]
