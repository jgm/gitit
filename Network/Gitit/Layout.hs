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
                    , formattedPage
                    )
where
import Data.FileStore
import Network.Gitit.Server
import Network.Gitit.Framework
import Network.Gitit.State
import Network.Gitit.Types
import Network.Gitit.Export (exportFormats)
import Network.HTTP (urlEncodeVars)
import Codec.Binary.UTF8.String (encodeString)
import qualified Text.StringTemplate as T
import Prelude hiding (catch)
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Exception (throwIO, catch)
import Control.Monad.Trans (liftIO, MonadIO)

defaultPageLayout :: PageLayout
defaultPageLayout = PageLayout
  { pgTitle          = ""
  , pgScripts        = []
  , pgShowPageTools  = True
  , pgShowSiteNav    = True
  , pgShowMarkupHelp = False
  , pgTabs           = [ViewTab, EditTab, HistoryTab, DiscussTab]
  , pgSelectedTab    = ViewTab
  }

-- | Returns formatted page
formattedPage :: PageLayout -> String -> Params -> Html -> GititServerPart Response
formattedPage layout page params htmlContents = do
  let rev = pRevision params
  fs <- getFileStore
  sha1 <- case (pgSelectedTab layout, rev) of
            (EditTab,Nothing) -> liftIO $ catch (latest fs $ pathForPage page)
                                                (\e -> if e == NotFound
                                                       then return ""
                                                       else throwIO e)
            (EditTab,Just r)  -> return r
            _ -> return ""
  user <- getLoggedInUser
  base' <- getWikiBase
  let scripts  = ["jquery.min.js", "jquery-ui.packed.js"] ++ pgScripts layout
  let scriptLink x = script ! [src (base' ++ "/_static/js/" ++ x),
        thetype "text/javascript"] << noHtml
  let javascriptlinks = if null (pgScripts layout)
                           then ""
                           else renderHtmlFragment $ concatHtml $
                                map scriptLink scripts
  let pageTitle' = pgTitle layout
  let tabli tab = if tab == pgSelectedTab layout
                     then li ! [theclass "selected"]
                     else li
  let tabs = ulist ! [theclass "tabs"] << map (linkForTab tabli base' page rev) (pgTabs layout)
  let searchbox = gui (base' ++ "/_search") ! [identifier "searchform"] <<
                         [ textfield "patterns"
                         , submit "search" "Search" ]
  let gobox     = gui (base' ++ "/_go") ! [identifier "goform"] <<
                         [ textfield "gotopage"
                         , submit "go" "Go" ]
  let messages = pMessages params
  let htmlMessages = if null messages
                        then noHtml
                        else ulist ! [theclass "messages"] <<
                          map (li <<) messages
  cfg <- getConfig
  templ <- getTemplate
  let filledTemp = T.render .
                   T.setAttribute "base" base' .
                   T.setAttribute "pagetitle" pageTitle' .
                   T.setAttribute "javascripts" javascriptlinks .
                   T.setAttribute "pagename" page .
                   (case user of
                         Just u     -> T.setAttribute "user" (uUsername u)
                         Nothing    -> id) .
                   (case authenticationMethod cfg of
                         FormAuth        -> T.setAttribute "showLogin" "true"
                         HTTPAuth        -> id
                         CustomAuth _    -> id) .
                   (if isPage page
                       then T.setAttribute "ispage" "true"
                       else id) .
                   (if pgShowPageTools layout
                       then T.setAttribute "pagetools" "true"
                       else id) .
                   (if pgShowSiteNav layout
                       then T.setAttribute "sitenav" "true"
                       else id) .
                   (if pgShowMarkupHelp layout
                       then T.setAttribute "markuphelp" (markupHelp cfg)
                       else id) .
                   (if pPrintable params
                       then T.setAttribute "printable" "true"
                       else id) .
                   (if isJust rev
                       then T.setAttribute "nothead" "true"
                       else id) .
                   (if isJust rev
                       then T.setAttribute "revision" (fromJust rev)
                       else id) .
                   (if null sha1
                       then id
                       else T.setAttribute "sha1" sha1) .
                   T.setAttribute "searchbox"
                       (renderHtmlFragment (searchbox +++ gobox)) .
                   T.setAttribute "exportbox"
                       (renderHtmlFragment $  exportBox base' page params) .
                   T.setAttribute "tabs" (renderHtmlFragment tabs) .
                   T.setAttribute "messages" (renderHtmlFragment htmlMessages) .
                   T.setAttribute "content" (renderHtmlFragment htmlContents) $
                   templ
  ok $ setContentType "text/html" $ toResponse $ encodeString filledTemp

exportBox :: String -> String -> Params -> Html
exportBox base' page params | not (isSourceCode page) =
  let rev = pRevision params
  in  gui (urlForPage base' page) ! [identifier "exportbox"] <<
        ([ textfield "revision" ! [thestyle "display: none;",
             value (fromJust rev)] | isJust rev ] ++
         [ select ! [name "format"] <<
             map ((\f -> option ! [value f] << f) . fst) exportFormats
         , submit "export" "Export" ])
exportBox _ _ _ = noHtml

-- auxiliary functions:

linkForTab :: (Tab -> Html -> Html) -> String -> String -> Maybe String -> Tab -> Html
linkForTab tabli base' page rev HistoryTab =
  tabli HistoryTab << anchor ! [href $ urlForPage base' page ++ "?history" ++
                                       case rev of
                                            Just r -> "&revision" ++ r
                                            Nothing -> "" ] << "history"
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

