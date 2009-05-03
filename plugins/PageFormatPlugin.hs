module PageFormatPlugin (plugin) where

-- This plugin looks at the first line of the stored file.
-- If it is '.. rst', the page is rendered as RST.
-- If it is '<!-- HTML -->', the page is rendered as HTML.
-- If it is '% LaTeX', the page is rendered as LaTeX.
-- If it is '<!-- markdown -->', the page is rendered as Markdown.
-- Otherwise, the default page format is used.

import Gitit.Interface

plugin :: Plugin
plugin = PreParseTransform checkFirstLine

checkFirstLine :: String -> PluginM String
checkFirstLine pg = do
  let firstLine = takeWhile (/='\n') pg
  case firstLine of
        ".. rst"            -> setPageType RST
        "<!-- HTML -->"     -> setPageType HTML 
        "% LaTeX"           -> setPageType LaTeX
        "<!-- markdown -->" -> setPageType Markdown
        _                   -> return ()
  return pg

setPageType :: PageType -> PluginM ()
setPageType pt = modifyContext $ \ctx -> ctx{ctxPageType = pt}

