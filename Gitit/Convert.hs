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

{- Functions for converting markup formats.
-}

module Gitit.Convert ( textToPandoc
                     , pandocToHtml
                     )
where
import Text.Pandoc
import Gitit.State
import Gitit.Server (Web)
import Control.Monad.Trans (MonadIO)
import Control.Monad (foldM)
import Text.XHtml
import Text.Pandoc.Shared (HTMLMathMethod(..))

{-
removeRawHtmlBlock :: Block -> Block
removeRawHtmlBlock (RawHtml _) = RawHtml "<!-- raw HTML removed -->"
removeRawHtmlBlock x = x
-}

readerFor :: PageType -> (String -> Pandoc)
readerFor pt = case pt of
                 RST      -> readRST (defaultParserState { stateSanitizeHTML = True, stateSmart = True })
                 Markdown -> readMarkdown (defaultParserState { stateSanitizeHTML = True, stateSmart = True })

textToPandoc :: PageType -> String -> Web Pandoc
textToPandoc pt s = do
  plugins' <- queryAppState plugins
  let plugins'' = wikiLinksPlugin : plugins'
  foldM (\d pl -> queryAppState id >>= \st -> pl st d) (readerFor pt $ filter (/= '\r') s) $ map transformation plugins''

wikiLinksPlugin = Plugin {
    description = "Converts links with blank url to wikilinks."
  , transformation = \_ d -> return $ processWith convertWikiLinks d }

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Inline -> Inline
convertWikiLinks (Link ref ("", "")) =
  Link ref (refToUrl ref, "Go to wiki page")
convertWikiLinks x = x

refToUrl :: [Inline] -> String
refToUrl = concatMap go
  where go (Str x)                  = x
        go (Space)                  = "%20"
        go (Quoted DoubleQuote xs)  = '"' : (refToUrl xs ++ "\"")
        go (Quoted SingleQuote xs)  = '\'' : (refToUrl xs ++ "'")
        go (Apostrophe)             = "'"
        go (Ellipses)               = "..."
        go (Math InlineMath t)      = '$' : (t ++ "$")
        go _                        = ""

-- | Converts pandoc document to HTML.
pandocToHtml :: MonadIO m => Pandoc -> m Html
pandocToHtml pandocContents = do
  cfg <- getConfig
  return $ writeHtml (defaultWriterOptions { writerStandalone = False
                                           , writerHTMLMathMethod = JsMath (Just "/js/jsMath/easy/load.js")
                                           , writerTableOfContents = tableOfContents cfg
                                           }) pandocContents

