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
import Text.Pandoc.ODT (saveOpenDocumentAsODT)
import Text.Pandoc.Shared (escapeStringUsing)
import Network.Gitit.Server
import Network.Gitit.Framework (pathForPage)
import Network.Gitit.Util (withTempDir)
import Network.Gitit.State (getConfig)
import Network.Gitit.Types
import Network.Gitit.Cache (cacheContents, lookupCache)
import Control.Monad.Trans (liftIO)
import Control.Monad (unless, when)
import Text.XHtml (noHtml)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
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
        -> (Pandoc -> String)
        -> String
        -> Pandoc
        -> Handler
respond mimetype ext fn page = ok . setContentType mimetype .
  (if null ext then id else setFilename (page ++ "." ++ ext)) .
  toResponse . fn

respondX :: String -> String -> String -> (WriterOptions -> Pandoc -> String)
          -> WriterOptions -> String -> Pandoc -> Handler
respondX templ mimetype ext fn opts page doc = do
  cfg <- getConfig
  template' <- liftIO $ getDefaultTemplate (pandocUserData cfg) templ
  template <- case template' of
                  Right t  -> return t
                  Left e   -> liftIO $ throwIO e
  respond mimetype ext (fn opts{writerTemplate = template}) page doc 

respondLaTeX :: String -> Pandoc -> Handler
respondLaTeX = respondX "latex" "application/x-latex" "tex"
  writeLaTeX defaultRespOptions

respondConTeXt :: String -> Pandoc -> Handler
respondConTeXt = respondX "context" "application/x-context" "tex"
  writeConTeXt defaultRespOptions


respondRTF :: String -> Pandoc -> Handler
respondRTF = respondX "rtf" "application/rtf" "rtf"
  writeRTF defaultRespOptions

respondRST :: String -> Pandoc -> Handler
respondRST = respondX "rst" "text/plain; charset=utf-8" ""
  writeRST defaultRespOptions{writerReferenceLinks = True}

respondMan :: String -> Pandoc -> Handler
respondMan = respondX "man" "text/plain; charset=utf-8" ""
  writeMan defaultRespOptions

respondS5 :: Config -> String -> Pandoc -> Handler
respondS5 cfg pg doc = do
  inc <- liftIO $ s5HeaderIncludes (pandocUserData cfg)
  respondX "s5" "text/html; charset=utf-8" ""
    writeS5String
    defaultRespOptions{writerS5 = True, writerIncremental = True,
                       writerVariables = [("header-includes",inc)]}
    pg doc

respondTexinfo :: String -> Pandoc -> Handler
respondTexinfo = respondX "texinfo" "application/x-texinfo" "texi"
  writeTexinfo defaultRespOptions

respondDocbook :: String -> Pandoc -> Handler
respondDocbook = respondX "docbook" "application/docbook+xml" "xml"
  writeDocbook defaultRespOptions

respondMediaWiki :: String -> Pandoc -> Handler
respondMediaWiki = respondX "mediawiki" "text/plain; charset=utf-8" ""
  writeMediaWiki defaultRespOptions

respondODT :: Config -> String -> Pandoc -> Handler
respondODT cfg page doc = do
  template' <- liftIO $ getDefaultTemplate (pandocUserData cfg)  "odt"
  template <-  case template' of
                  Right t  -> return t
                  Left e   -> liftIO $ throwIO e
  let openDoc = writeOpenDocument
                defaultRespOptions{writerTemplate = template}
                doc
  conf <- getConfig
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                saveOpenDocumentAsODT (pandocUserData cfg) 
                   tempfile (repositoryPath conf) Nothing openDoc
                L.readFile tempfile
  ok $ setContentType "application/vnd.oasis.opendocument.text" $
       setFilename (page ++ ".odt") $ (toResponse noHtml) {rsBody = contents}

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
respondPDF page pndc = do
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
              let tempfile = page <.> "tex"
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
                  ExitSuccess   -> do pdfBS <- L.readFile (tempdir </> page <.> "pdf")
                                      return $ Right (useCache cfg, pdfBS)
                  ExitFailure n -> do l <- readFile (tempdir </> page <.> "log")
                                      return $ Left (n, l)
  case pdf' of
       Left (n,logOutput) -> simpleErrorHandler ("PDF creation failed with code: " ++
                               show n ++ "\n" ++ logOutput)
       Right (needsCaching, pdfBS) -> do
              when needsCaching $
                 cacheContents cacheName $ B.concat . L.toChunks $ pdfBS
              ok $ setContentType "application/pdf" $ setFilename (page ++ ".pdf") $
                        (toResponse noHtml) {rsBody = pdfBS}

exportFormats :: Config -> [(String, String -> Pandoc -> Handler)]
exportFormats cfg = if pdfExport cfg
                       then ("PDF", respondPDF) : rest
                       else rest
   where rest = [ ("LaTeX",     respondLaTeX)     -- (description, writer)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("MediaWiki", respondMediaWiki)
                , ("man",       respondMan)
                , ("DocBook",   respondDocbook)
                , ("S5",        respondS5 cfg)
                , ("ODT",       respondODT cfg )
                , ("RTF",       respondRTF) ]
