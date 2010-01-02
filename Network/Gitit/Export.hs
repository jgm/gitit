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

module Network.Gitit.Export ( exportFormats )
where
import Text.Pandoc
import Text.Pandoc.Writers.S5 (s5HeaderIncludes)
import Text.Pandoc.ODT (saveOpenDocumentAsODT)
import Network.Gitit.Server
import Network.Gitit.Util (withTempDir)
import Network.Gitit.State
import Network.Gitit.Types
import Control.Monad.Trans (liftIO)
import Text.XHtml (noHtml)
import qualified Data.ByteString.Lazy as B
import System.FilePath ((<.>), (</>))
import Control.Exception (throwIO)

defaultRespOptions :: WriterOptions
defaultRespOptions = defaultWriterOptions { writerStandalone = True
                                          , writerWrapText = True }

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
  template' <- liftIO $ getDefaultTemplate templ
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

respondS5 :: String -> Pandoc -> Handler
respondS5 pg doc = do
  inc <- liftIO $ s5HeaderIncludes
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

respondODT :: String -> Pandoc -> Handler
respondODT page doc = do
  template' <- liftIO $ getDefaultTemplate "odt"
  template <-  case template' of
                  Right t  -> return t
                  Left e   -> liftIO $ throwIO e
  let openDoc = writeOpenDocument
                defaultRespOptions{writerTemplate = template}
                doc
  conf <- getConfig
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                saveOpenDocumentAsODT tempfile (repositoryPath conf) Nothing openDoc
                B.readFile tempfile
  ok $ setContentType "application/vnd.oasis.opendocument.text" $
       setFilename (page ++ ".odt") $ (toResponse noHtml) {rsBody = contents}

exportFormats :: [(String, String -> Pandoc -> Handler)]
exportFormats = [ ("LaTeX",     respondLaTeX)     -- (description, writer)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("MediaWiki", respondMediaWiki)
                , ("man",       respondMan)
                , ("DocBook",   respondDocbook)
                , ("S5",        respondS5)
                , ("ODT",       respondODT)
                , ("RTF",       respondRTF) ]
