module PageFormatPlugin (plugin) where

-- This plugin looks at the first line of the stored file.
-- If it is '.. rst', the page is rendered as RST.
-- If it is '.. rst+lhs' or '.. literate haskell',
-- the page is rendered as RST literate Haskell.
-- If it is '<!-- HTML -->', the page is rendered as HTML.
-- If it is '% LaTeX', the page is rendered as LaTeX.
-- If it is '% LaTeX+lhs' or '% lhs' or '% literate haskell',
-- the page is rendered as LaTeX literate Haskell.
-- If it is '<!-- markdown -->', the page is rendered as Markdown.
-- If it is '<!-- markdown+lhs -->' or '<!-- lhs -->' or '<!-- literate haskell -->',
-- the page is rendered as Markdown literate Haskell.
-- Otherwise, the default page format is used.

import Network.Gitit.Interface
import Data.Char (toLower)

plugin :: Plugin
plugin = PreParseTransform checkFirstLine

checkFirstLine :: String -> PluginM String
checkFirstLine pg = do
  let firstLine = map toLower $ takeWhile (/='\n') pg
  case firstLine of
        ".. rst"            -> setPageType (RST, False)
        ".. rst+lhs"        -> setPageType (RST, True)
        ".. lhs"            -> setPageType (RST, True)
        ".. literate haskell" -> setPageType (RST, True)
        "<!-- html -->"     -> setPageType (HTML, False)
        "% latex"           -> setPageType (LaTeX, False)
        "% latex+lhs"       -> setPageType (LaTeX, True)
        "% lhs"             -> setPageType (LaTeX, True)
        "% literate haskell" -> setPageType (LaTeX, True)
        "<!-- markdown -->" -> setPageType (Markdown, False)
        "<!-- markdown+lhs -->" -> setPageType (Markdown, True)
        "<!-- lhs -->"      -> setPageType (Markdown, True)
        "<!-- literate haskell -->" -> setPageType (Markdown, True)
        _                   -> return ()
  return pg

setPageType :: (PageType, Bool) -> PluginM ()
setPageType (pt,lhs) = modifyContext $ \ctx ->
  ctx{ctxPageType = pt, ctxLiterateHaskell = lhs}
