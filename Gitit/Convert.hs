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
                     , inlinesToURL
                     , inlinesToString
                     )
where
import Text.Pandoc
import Gitit.State
import Gitit.Server (Web)
import Control.Monad.Trans (MonadIO)
import Control.Monad (foldM)
import Text.XHtml
import Text.Pandoc.Shared (HTMLMathMethod(..))
import Data.Generics (everywhere, mkT)
import Network.URI (isAllowedInURI, escapeURIString)

readerFor :: PageType -> (String -> Pandoc)
readerFor pt = case pt of
                 RST      -> readRST (defaultParserState { stateSanitizeHTML = True, stateSmart = True })
                 Markdown -> readMarkdown (defaultParserState { stateSanitizeHTML = True, stateSmart = True })

textToPandoc :: PageType -> String -> Web Pandoc
textToPandoc pt s = do
  transforms <- getPageTransforms
  foldM (\d pl -> queryAppState id >>= \st -> pl st d) (readerFor pt $ filter (/= '\r') s) (wikiLinksTransform : transforms)

wikiLinksTransform :: AppState -> Pandoc -> Web Pandoc
wikiLinksTransform _ = return . everywhere (mkT convertWikiLinks)

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Inline -> Inline
convertWikiLinks (Link ref ("", "")) =
  Link ref (inlinesToURL ref, "Go to wiki page")
convertWikiLinks x = x

inlinesToURL :: [Inline] -> String
inlinesToURL = escapeURIString isAllowedInURI  . inlinesToString

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

-- | Converts pandoc document to HTML.
pandocToHtml :: MonadIO m => Pandoc -> m Html
pandocToHtml pandocContents = do
  cfg <- getConfig
  return $ writeHtml (defaultWriterOptions { writerStandalone = False
                                           , writerHTMLMathMethod = JsMath (Just "/js/jsMath/easy/load.js")
                                           , writerTableOfContents = tableOfContents cfg
                                           }) pandocContents

