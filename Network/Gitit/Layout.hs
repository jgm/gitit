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
  , pgShowMarkupHelp = False
  , pgTabs           = [ViewTab, EditTab, HistoryTab, DiscussTab]
  , pgSelectedTab    = ViewTab
  }

-- | Returns formatted page
formattedPage :: PageLayout -> Html -> Handler
formattedPage layout htmlContents = do
  renderer <- queryGititState renderPage
  renderer layout htmlContents

-- | Given a compiled string template, returns a page renderer.
defaultRenderPage :: T.StringTemplate String -> PageLayout -> Html -> Handler
defaultRenderPage templ layout htmlContents = do
  let rev  = pgRevision layout
  let page = pgPageName layout
  user <- getLoggedInUser
  base' <- getWikiBase
  let scripts  = ["jquery.min.js", "jquery-ui.packed.js"] ++ pgScripts layout
  let scriptLink x = script ! [src (base' ++ "/_static/js/" ++ x),
        thetype "text/javascript"] << noHtml
  let javascriptlinks = renderHtmlFragment $ concatHtml $ map scriptLink scripts
  let pageTitle' = pgTitle layout
  let tabli tab = if tab == pgSelectedTab layout
                     then li ! [theclass "selected"]
                     else li
  let tabs = ulist ! [theclass "tabs"] << map (linkForTab tabli base' page rev) (pgTabs layout)
  cfg <- getConfig
  let setStrAttr  attr = T.setAttribute attr . stringToHtmlString
  let setBoolAttr attr test = if test then T.setAttribute attr "true" else id
  let filledTemp = T.render .
                   T.setAttribute "base" base' .
                   setStrAttr "pagetitle" pageTitle' .
                   T.setAttribute "javascripts" javascriptlinks .
                   setStrAttr "pagename" page .
                   (case user of
                         Just u     -> setStrAttr "user" (uUsername u)
                         Nothing    -> id) .
                   setBoolAttr "ispage" (isPage page) .
                   setBoolAttr "pagetools" (pgShowPageTools layout) .
                   setBoolAttr "sitenav" (pgShowSiteNav layout) .
                   (if pgShowMarkupHelp layout
                       then T.setAttribute "markuphelp" (markupHelp cfg)
                       else id) .
                   setBoolAttr "printable" (pgPrintable layout) .
                   setBoolAttr "nothead" (isJust rev) .
                   (if isJust rev
                       then T.setAttribute "revision" (fromJust rev)
                       else id) .
                   T.setAttribute "exportbox"
                       (renderHtmlFragment $  exportBox base' page rev) .
                   T.setAttribute "tabs" (renderHtmlFragment tabs) .
                   T.setAttribute "messages" (pgMessages layout) .
                   T.setAttribute "content" (renderHtmlFragment htmlContents) $
                   templ
  ok $ setContentType "text/html" $ toResponse filledTemp

exportBox :: String -> String -> Maybe String -> Html
exportBox base' page rev | not (isSourceCode page) =
  gui (urlForPage base' page) ! [identifier "exportbox"] <<
    ([ textfield "revision" ! [thestyle "display: none;",
         value (fromJust rev)] | isJust rev ] ++
     [ select ! [name "format"] <<
         map ((\f -> option ! [value f] << f) . fst) exportFormats
     , submit "export" "Export" ])
exportBox _ _ _ = noHtml

-- auxiliary functions:

linkForTab :: (Tab -> Html -> Html) -> String -> String -> Maybe String -> Tab -> Html
linkForTab tabli base' page _ HistoryTab =
  tabli HistoryTab << anchor ! [href $ urlForPage base' page ++ "?history"] << "history"
linkForTab tabli _ _ _ DiffTab =
  tabli DiffTab << anchor ! [href ""] << "diff"
linkForTab tabli base' page rev ViewTab =
  let origPage s = if isDiscussPage s
                      then drop 1 s
                      else s
  in if isDiscussPage page
        then tabli DiscussTab << anchor !
              [href $ urlForPage base' $ origPage page] << "page"
        else tabli ViewTab << anchor !
              [href $ urlForPage base' page ++
                      case rev of
                           Just r  -> "?revision=" ++ r
                           Nothing -> ""] << "view"
linkForTab tabli base' page _ DiscussTab =
  tabli (if isDiscussPage page then ViewTab else DiscussTab) <<
  anchor ! [href $ urlForPage base' page ++
            if isDiscussPage page then "" else "?discuss"] << "discuss"
linkForTab tabli base' page rev EditTab =
  tabli EditTab << anchor !
    [href $ urlForPage base' page ++ "?edit" ++
            case rev of
                  Just r   -> "&revision=" ++ r ++ "&" ++
                               urlEncodeVars [("logMsg", "Revert to " ++ r)]
                  Nothing  -> ""] << if isNothing rev
                                         then "edit"
                                         else "revert"

