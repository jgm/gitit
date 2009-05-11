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

module Gitit.Layout ( defaultPageLayout
                    , formattedPage
                    )
where
import Data.FileStore
import Gitit.Server
import Gitit.Framework
import Gitit.State
import Gitit.Types
import Gitit.Util (orIfNull)
import Gitit.Export (exportFormats)
import Network.HTTP (urlEncodeVars)
import Codec.Binary.UTF8.String (encodeString)
import qualified Text.StringTemplate as T
import Text.XHtml hiding ( (</>), dir, method, password, rev )
import Data.Maybe (isNothing, isJust, fromJust)
import Data.List (isSuffixOf)
import Prelude hiding (catch)
import Control.Exception (throwIO, catch)
import Control.Monad.Trans (liftIO)

defaultPageLayout :: PageLayout
defaultPageLayout = PageLayout
  { pgTitle          = ""
  , pgScripts        = []
  , pgShowPageTools  = True
  , pgTabs           = [ViewTab, EditTab, HistoryTab, DiscussTab]
  , pgSelectedTab    = ViewTab
  }

-- | Returns formatted page
formattedPage :: PageLayout -> String -> Params -> Html -> Web Response
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
  user <- getLoggedInUser params
  let scripts  = ["jquery.min.js", "jquery-ui.packed.js"] ++ pgScripts layout
  let scriptLink x = script ! [src ("/_static/js/" ++ x),
        thetype "text/javascript"] << noHtml
  let javascriptlinks = if null (pgScripts layout)
                           then ""
                           else renderHtmlFragment $ concatHtml $
                                map scriptLink scripts
  let pageTitle = pgTitle layout `orIfNull` page
  let tabli tab = if tab == pgSelectedTab layout
                     then li ! [theclass "selected"]
                     else li
  let tabs = ulist ! [theclass "tabs"] << map (linkForTab tabli page rev) (pgTabs layout)
  let searchbox = gui "/_search" ! [identifier "searchform"] <<
                         [ textfield "patterns"
                         , submit "search" "Search" ]
  let gobox     = gui "/_go" ! [identifier "goform"] <<
                         [ textfield "gotopage"
                         , submit "go" "Go" ]
  let messages = pMessages params
  let htmlMessages = if null messages
                        then noHtml
                        else ulist ! [theclass "messages"] <<
                          map (li <<) messages
  templ <- queryAppState template
  cfg <- getConfig
  let filledTemp = T.render .
                   T.setAttribute "pagetitle" pageTitle .
                   T.setAttribute "javascripts" javascriptlinks .
                   T.setAttribute "pagename" page .
                   (case user of
                         Just u     -> T.setAttribute "user" u
                         Nothing    -> id) .
                   (case authenticationMethod cfg of
                         FormAuth        -> T.setAttribute "showLogin" "true"
                         HTTPDigestAuth  -> T.setAttribute "showLogin" "false") .
                   (if isPage page
                       then T.setAttribute "ispage" "true"
                       else id) .
                   (if pgShowPageTools layout
                       then T.setAttribute "pagetools" "true"
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
                       (renderHtmlFragment $  exportBox page params) .
                   T.setAttribute "tabs" (renderHtmlFragment tabs) .
                   T.setAttribute "messages" (renderHtmlFragment htmlMessages) .
                   T.setAttribute "content" (renderHtmlFragment htmlContents) $
                   templ
  ok $ setContentType "text/html" $ toResponse $ encodeString filledTemp

exportBox :: String -> Params -> Html
exportBox page params | not (isSourceCode page) =
  let rev = pRevision params
  in  gui (urlForPage page) ! [identifier "exportbox"] <<
        ([ textfield "revision" ! [thestyle "display: none;",
             value (fromJust rev)] | isJust rev ] ++
         [ select ! [name "format"] <<
             map ((\f -> option ! [value f] << f) . fst) exportFormats
         , submit "export" "Export" ])
exportBox _ _ = noHtml

-- auxiliary functions:

linkForTab :: (Tab -> Html -> Html) -> String -> Maybe String -> Tab -> Html
linkForTab tabli page rev HistoryTab =
  tabli HistoryTab << anchor ! [href $ urlForPage page ++ "?history" ++
                                       case rev of
                                            Just r -> "&revision" ++ r
                                            Nothing -> "" ] << "history"
linkForTab tabli _ _ DiffTab =
  tabli DiffTab << anchor ! [href ""] << "diff"
linkForTab tabli page rev ViewTab =
  let origPage s = if ":discuss" `isSuffixOf` s
                      then take (length s - 8) s
                      else s
  in if isDiscussPage page
        then tabli DiscussTab << anchor !
              [href $ urlForPage $ origPage page] << "page"
        else tabli ViewTab << anchor !
              [href $ urlForPage page ++
                      case rev of
                           Just r  -> "?revision=" ++ r
                           Nothing -> ""] << "view"
linkForTab tabli page _ DiscussTab =
  tabli (if isDiscussPage page then ViewTab else DiscussTab) <<
  anchor ! [href $ urlForPage page ++
            if isDiscussPage page then "" else "?discuss"] << "discuss"
linkForTab tabli page rev EditTab =
  tabli EditTab << anchor !
    [href $ urlForPage page ++ "?edit" ++
            case rev of
                  Just r   -> "&revision=" ++ r ++ "&" ++
                               urlEncodeVars [("logMsg", "Revert to " ++ r)]
                  Nothing  -> ""] << if isNothing rev
                                         then "edit"
                                         else "revert"

