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
defaultRespOptions = defaultWriterOptions { writerStandalone = True, writerWrapText = True }

respondLaTeX :: String -> Pandoc -> Web Response
respondLaTeX page = ok . setContentType "application/x-latex" . setFilename (page ++ ".tex") . toResponse . encodeString .
                    writeLaTeX (defaultRespOptions {writerHeader = defaultLaTeXHeader})

respondConTeXt :: String -> Pandoc -> Web Response
respondConTeXt page = ok . setContentType "application/x-context" . setFilename (page ++ ".tex") . toResponse . encodeString .
                      writeConTeXt (defaultRespOptions {writerHeader = defaultConTeXtHeader})

respondRTF :: String -> Pandoc -> Web Response
respondRTF page = ok . setContentType "application/rtf" . setFilename (page ++ ".rtf") . toResponse . encodeString .
                  writeRTF (defaultRespOptions {writerHeader = defaultRTFHeader})

respondRST :: String -> Pandoc -> Web Response
respondRST _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . encodeString .
               writeRST (defaultRespOptions {writerHeader = "", writerReferenceLinks = True})

respondMan :: String -> Pandoc -> Web Response
respondMan _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . encodeString .
               writeMan (defaultRespOptions {writerHeader = ""})

respondS5 :: String -> Pandoc -> Web Response
respondS5 _ = ok . toResponse . writeS5 (defaultRespOptions {writerHeader = defaultS5Header,
                                                             writerS5 = True, writerIncremental = True})

respondTexinfo :: String -> Pandoc -> Web Response
respondTexinfo page = ok . setContentType "application/x-texinfo" . setFilename (page ++ ".texi") . toResponse . encodeString .
                      writeTexinfo (defaultRespOptions {writerHeader = ""})

respondDocbook :: String -> Pandoc -> Web Response
respondDocbook page = ok . setContentType "application/docbook+xml" . setFilename (page ++ ".xml") . toResponse . encodeString .
                      writeDocbook (defaultRespOptions {writerHeader = defaultDocbookHeader})

respondMediaWiki :: String -> Pandoc -> Web Response
respondMediaWiki _ = ok . setContentType "text/plain; charset=utf-8" . toResponse . encodeString .
                     writeMediaWiki (defaultRespOptions {writerHeader = ""})

respondODT :: String -> Pandoc -> Web Response
respondODT page doc = do
  let openDoc = writeOpenDocument (defaultRespOptions {writerHeader = defaultOpenDocumentHeader}) doc
  contents <- liftIO $ withTempDir "gitit-temp-odt" $ \tempdir -> do
                let tempfile = tempdir </> page <.> "odt"
                conf <- getConfig
                let repoPath = case repository conf of
                                Git path'   -> path'
                                Darcs path' -> path'
                saveOpenDocumentAsODT tempfile repoPath openDoc
                B.readFile tempfile
  ok $ setContentType "application/vnd.oasis.opendocument.text" $ setFilename (page ++ ".odt") $ (toResponse noHtml) {rsBody = contents}

exportFormats :: [(String, String -> Pandoc -> Web Response)]   -- (description, writer)
exportFormats = [ ("LaTeX",     respondLaTeX)
                , ("ConTeXt",   respondConTeXt)
                , ("Texinfo",   respondTexinfo)
                , ("reST",      respondRST)
                , ("MediaWiki", respondMediaWiki)
                , ("man",       respondMan)
                , ("DocBook",   respondDocbook)
                , ("S5",        respondS5)
                , ("ODT",       respondODT)
                , ("RTF",       respondRTF) ]


