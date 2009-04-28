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

module Gitit.Export ( exportFormats )
where
import Text.Pandoc
import Text.Pandoc.ODT (saveOpenDocumentAsODT)
import Gitit.Server
import Gitit.Util (withTempDir)
import Gitit.State
import Control.Monad.Trans (liftIO)
import Text.XHtml (noHtml)
import qualified Data.ByteString.Lazy as B
import System.FilePath ((<.>), (</>))
import Codec.Binary.UTF8.String (encodeString)

defaultRespOptions :: WriterOptions
defaultRespOptions = defaultWriterOptions { writerStandalone = True
                                          , writerWrapText = True }

respond :: String
        -> String
        -> (Pandoc -> String)
        -> String
        -> Pandoc
        -> Web Response
respond mimetype ext fn page = ok . setContentType mimetype .
  (if null ext then id else setFilename (page ++ "." ++ ext)) .
  toResponse . encodeString . fn

respondLaTeX :: String -> Pandoc -> Web Response
respondLaTeX = respond "application/x-latex" "tex" $
  writeLaTeX (defaultRespOptions {writerHeader = defaultLaTeXHeader})

respondConTeXt :: String -> Pandoc -> Web Response
respondConTeXt = respond "application/x-context" "tex" $
  writeConTeXt (defaultRespOptions {writerHeader = defaultConTeXtHeader})

respondRTF :: String -> Pandoc -> Web Response
respondRTF = respond "application/rtf" "rtf" $
  writeRTF (defaultRespOptions {writerHeader = defaultRTFHeader})

respondRST :: String -> Pandoc -> Web Response
respondRST = respond "text/plain; charset=utf-8" "" $
  writeRST (defaultRespOptions {writerHeader = "", writerReferenceLinks = True})

respondMan :: String -> Pandoc -> Web Response
respondMan = respond "text/plain; charset=utf-8" "" $
  writeMan (defaultRespOptions {writerHeader = ""})

respondS5 :: String -> Pandoc -> Web Response
respondS5 _ = ok . toResponse .
  writeS5 (defaultRespOptions {writerHeader = defaultS5Header,
            writerS5 = True, writerIncremental = True})

respondTexinfo :: String -> Pandoc -> Web Response
respondTexinfo = respond "application/x-texinfo" "texi" $
  writeTexinfo (defaultRespOptions {writerHeader = ""})

respondDocbook :: String -> Pandoc -> Web Response
respondDocbook = respond "application/docbook+xml" "xml" $
  writeDocbook (defaultRespOptions {writerHeader = defaultDocbookHeader})

respondMediaWiki :: String -> Pandoc -> Web Response
respondMediaWiki = respond "text/plain; charset=utf-8" "" $
  writeMediaWiki (defaultRespOptions {writerHeader = ""})

respondODT :: String -> Pandoc -> Web Response
respondODT page doc = do
  let openDoc = writeOpenDocument
                (defaultRespOptions {writerHeader = defaultOpenDocumentHeader})
                doc
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                conf <- getConfig
                let repoPath = case repository conf of
                                Git path'   -> path'
                                Darcs path' -> path'
                saveOpenDocumentAsODT tempfile repoPath openDoc
                B.readFile tempfile
  ok $ setContentType "application/vnd.oasis.opendocument.text" $
       setFilename (page ++ ".odt") $ (toResponse noHtml) {rsBody = contents}

exportFormats :: [(String, String -> Pandoc -> Web Response)]
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
