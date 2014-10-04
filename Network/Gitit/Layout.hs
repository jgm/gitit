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

{- Functions and data structures for wiki page layout.
-}

module Network.Gitit.Layout ( defaultPageLayout
                            , defaultRenderPage
                            , formattedPage
                            , filledPageTemplate
                            , uploadsAllowed
                            )
where
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Export (exportFormats)
import Network.HTTP (urlEncodeVars)
import qualified Text.StringTemplate as T
import Prelude hiding (catch)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import Text.XHtml.Strict ( stringToHtmlString )
import Data.Maybe (isNothing, isJust, fromJust)

defaultPageLayout :: PageLayout
defaultPageLayout = PageLayout
  { pgPageName       = ""
  , pgRevision       = Nothing
  , pgPrintable      = False
  , pgMessages       = []
  , pgTitle          = ""
  , pgScripts        = []
  , pgShowPageTools  = True
  , pgShowSiteNav    = True
  , pgMarkupHelp     = Nothing
  , pgTabs           = [ViewTab, EditTab, HistoryTab, DiscussTab]
  , pgSelectedTab    = ViewTab
  , pgLinkToFeed     = False
  }

-- | Returns formatted page
formattedPage :: PageLayout -> Html -> Handler
formattedPage layout htmlContents = do
  renderer <- queryGititState renderPage
  renderer layout htmlContents

-- | Given a compiled string template, returns a page renderer.
defaultRenderPage :: T.StringTemplate String -> PageLayout -> Html -> Handler
defaultRenderPage templ layout htmlContents = do
  cfg <- getConfig
  base' <- getWikiBase
  ok . setContentType "text/html; charset=utf-8" . toResponse . T.render .
       filledPageTemplate base' cfg layout htmlContents $ templ

-- | Returns a page template with gitit variables filled in.
filledPageTemplate :: String -> Config -> PageLayout -> Html ->
                      T.StringTemplate String -> T.StringTemplate String 
filledPageTemplate base' cfg layout htmlContents templ = 
  let rev  = pgRevision layout
      page = pgPageName layout
      scripts  = ["jquery.min.js", "jquery-ui.packed.js"] ++ pgScripts layout
      scriptLink x = script ! [src (base' ++ "/js/" ++ x),
        thetype "text/javascript"] << noHtml
      javascriptlinks = renderHtmlFragment $ concatHtml $ map scriptLink scripts
      tabli tab = if tab == pgSelectedTab layout
                     then li ! [theclass "selected"]
                     else li
      tabs' = [x | x <- pgTabs layout,
                not (x == EditTab && page `elem` noEdit cfg)]
      tabs = ulist ! [theclass "tabs"] << map (linkForTab tabli base' page rev) tabs'
      setStrAttr  attr = T.setAttribute attr . stringToHtmlString
      setBoolAttr attr test = if test then T.setAttribute attr "true" else id
  in               T.setAttribute "base" base' .
                   T.setAttribute "feed" (pgLinkToFeed layout) .
                   setStrAttr "wikititle" (wikiTitle cfg) .
                   setStrAttr "pagetitle" (pgTitle layout) .
                   T.setAttribute "javascripts" javascriptlinks .
                   setStrAttr "pagename" page .
                   setStrAttr "pageUrl" (urlForPage page) .
                   setBoolAttr "ispage" (isPage page) .
                   setBoolAttr "pagetools" (pgShowPageTools layout) .
                   setBoolAttr "sitenav" (pgShowSiteNav layout) .
                   maybe id (T.setAttribute "markuphelp") (pgMarkupHelp layout) .
                   setBoolAttr "printable" (pgPrintable layout) .
                   maybe id (T.setAttribute "revision") rev .
                   T.setAttribute "exportbox"
                       (renderHtmlFragment $  exportBox base' cfg page rev) .
                   T.setAttribute "tabs" (renderHtmlFragment tabs) .
                   T.setAttribute "messages" (pgMessages layout) .
                   T.setAttribute "usecache" (useCache cfg) .
                   T.setAttribute "content" (renderHtmlFragment htmlContents) . 
                   setBoolAttr "wikiupload" ( uploadsAllowed cfg) $
                   templ



exportBox :: String -> Config -> String -> Maybe String -> Html
exportBox base' cfg page rev | not (isSourceCode page) =
  gui (base' ++ urlForPage page) ! [identifier "exportbox"] <<
    ([ textfield "revision" ! [thestyle "display: none;",
         value (fromJust rev)] | isJust rev ] ++
     [ select ! [name "format"] <<
         map ((\f -> option ! [value f] << f) . fst) (exportFormats cfg)
     , primHtmlChar "nbsp" 
     , submit "export" "Export" ])
exportBox _ _ _ _ = noHtml

-- auxiliary functions:

linkForTab :: (Tab -> Html -> Html) -> String -> String -> Maybe String -> Tab -> Html
linkForTab tabli base' page _ HistoryTab =
  tabli HistoryTab << anchor ! [href $ base' ++ "/_history" ++ urlForPage page] << "history"
linkForTab tabli _ _ _ DiffTab =
  tabli DiffTab << anchor ! [href ""] << "diff"
linkForTab tabli base' page rev ViewTab =
  let origPage s = if isDiscussPage s
                      then drop 1 s
                      else s
  in if isDiscussPage page
        then tabli DiscussTab << anchor !
              [href $ base' ++ urlForPage (origPage page)] << "page"
        else tabli ViewTab << anchor !
              [href $ base' ++ urlForPage page ++
                      case rev of
                           Just r  -> "?revision=" ++ r
                           Nothing -> ""] << "view"
linkForTab tabli base' page _ DiscussTab =
  tabli (if isDiscussPage page then ViewTab else DiscussTab) <<
  anchor ! [href $ base' ++ if isDiscussPage page then "" else "/_discuss" ++
                   urlForPage page] << "discuss"
linkForTab tabli base' page rev EditTab =
  tabli EditTab << anchor !
    [href $ base' ++ "/_edit" ++ urlForPage page ++
            case rev of
                  Just r   -> "?revision=" ++ r ++ "&" ++
                               urlEncodeVars [("logMsg", "Revert to " ++ r)]
                  Nothing  -> ""] << if isNothing rev
                                         then "edit"
                                         else "revert"

uploadsAllowed :: Config -> Bool
uploadsAllowed = (0 <) . maxUploadSize
