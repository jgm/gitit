{-
Copyright (C) 2009 Gwern Branwen <gwern0@gmail.com> and
John MacFarlane <jgm@berkeley.edu>

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

{- Functions for creating atom feeds for gitit wikis and pages.
-}

module Network.Gitit.Feed (FeedConfig(..), filestoreToXmlFeed) where

import Control.Monad
import Data.DateTime
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Ord (comparing)
import Network.URI (isAllowedInURI, escapeURIString)
import System.FilePath

import Data.FileStore.Types

import Text.Atom.Feed
import Text.Atom.Feed.Export
import Text.XML.Light

data FeedConfig = FeedConfig {
    fcTitle    :: String
  , fcBaseUrl  :: String
  , fcFeedDays :: Integer
  } deriving (Show, Read)

filestoreToXmlFeed :: FeedConfig -> FileStore -> (Maybe FilePath) -> IO String
filestoreToXmlFeed cfg f mbPath = filestoreToFeed cfg f mbPath >>= return . ppTopElement . xmlFeed

filestoreToFeed :: FeedConfig -> FileStore -> (Maybe FilePath) -> IO Feed
filestoreToFeed cfg a mbPath = do
  let path' = maybe "" id mbPath
  when (null $ fcBaseUrl cfg) $ error "base-url in the config file is null."
  rs <- changeLog cfg a mbPath
{-  let rsShifted = if null rs
                     then []
                     else head rs : init rs   -- so we can get revids for diffs
-}
  let rsShifted = case rs of
                     [] -> []
                     (x:_) -> x : init rs   -- so we can get revids for diffs
  now <- liftM formatFeedTime getCurrentTime
  return $ Feed { feedId = fcBaseUrl cfg ++ "/" ++ escape path'
                , feedTitle = TextString $ fcTitle cfg
                , feedUpdated = now
                , feedAuthors = []
                , feedCategories = []
                , feedContributors = []
                , feedGenerator = Just Generator{ genURI = Just "http://github.com/jgm/gitit"
                                                , genVersion = Nothing
                                                , genText = "gitit" }
                , feedIcon = Nothing
                , feedLinks = [ (nullLink (fcBaseUrl cfg ++ "/_feed/" ++ escape path')) {linkRel = Just (Left "self")} ]
                , feedLogo = Nothing
                , feedRights = Nothing
                , feedSubtitle = Nothing
                , feedAttrs = []
                , feedOther = []  
                , feedEntries = reverse $ zipWith (revToEntry cfg path') rs rsShifted }

-- | Get the last N days history.
changeLog :: FeedConfig -> FileStore -> (Maybe FilePath) -> IO [Revision]
changeLog cfg a mbPath = do
  let files = maybe [] (\f -> [f, f <.> "page"]) mbPath
  now <- getCurrentTime
  let startTime = addMinutes (-60 * 24 * fcFeedDays cfg) now
  rs <- history a files TimeRange{timeFrom = Just startTime, timeTo = Just now}
  return $ sortBy (comparing revDateTime) rs
 
revToEntry :: FeedConfig -> String -> Revision -> Revision -> Entry
revToEntry cfg path' Revision{
                     revId = rid,
                     revDateTime = rdt,
                     revAuthor = ra,
                     revDescription = rd,
                     revChanges = rv } prevRevision =
  baseEntry{ entrySummary = Just $ TextString rd
           , entryAuthors = [Person { personName = authorName ra
                                    , personURI = Nothing 
                                    , personEmail = Nothing
                                      -- gitit is set up not to reveal registration emails. To change this:
                                      -- let e = authorEmail ra in if e /= "" then Just e else Nothing
                                    , personOther = [] }]
           , entryLinks = [diffLink]

           -- Comments omitted; needs to be done by Gitit
           -- only Gitit knows the Url of the Talk: page. See
           -- http://www.rssboard.org/rss-2-0-1-rv-6#ltcommentsgtSubelementOfLtitemgt

           -- FIXME: True field seems to tell Guid that it's a 'long-term'/'permanent'
           -- GUID. This may not be correct. See
           -- https://secure.wikimedia.org/wikipedia/en/wiki/Globally_Unique_Identifier
           -- entryId = rid,

           -- Source is not entirely relevant, and is only handleable by web software,
           -- not by a filestore-level function. See
           -- http://www.rssboard.org/rss-2-0-1-rv-6#ltsourcegtSubelementOfLtitemgt

           -- The following are omitted:
           -- Category is omitted, see
           -- http://www.rssboard.org/rss-2-0-1-rv-6#syndic8
           -- Enclosure seems to be for conveying media, see
           -- https://secure.wikimedia.org/wikipedia/en/wiki/RSS_enclosure
        }
    where diffLink = Link{ linkHref = fcBaseUrl cfg ++ "/_diff/" ++ escape firstpath ++ "?to=" ++ rid ++ fromrev
                         , linkRel = Just (Left "alternate")
                         , linkType = Nothing
                         , linkHrefLang = Nothing
                         , linkTitle = Nothing
                         , linkLength = Nothing
                         , linkAttrs = []
                         , linkOther = [] } 
          (firstpath, fromrev) =
                      if null path'
                         {- then case head rv of
                                   Modified f -> (dePage f, "&from=" ++ revId prevRevision)
                                   Added f    -> (dePage f, "")
                                   Deleted f  -> (dePage f, "&from=" ++ revId prevRevision)
                         else (path',"") -}
                         then case rv of
                                [] -> error "revToEntry, null rv"
                                (rev:_) -> case rev of 
                                   Modified f -> (dePage f, "&from=" ++ revId prevRevision)                                
                                   Added f    -> (dePage f, "")
                                   Deleted f  -> (dePage f, "&from=" ++ revId prevRevision)
                         else (path',"")
          baseEntry = nullEntry (fcBaseUrl cfg ++ "/" ++ escape path' ++ "?revision=" ++ rid)
                        (TextString (intercalate ", " $ map showRev rv)) (formatFeedTime rdt)
          showRev (Modified f) = dePage f
          showRev (Added f)    = "added " ++ dePage f
          showRev (Deleted f)  = "deleted " ++ dePage f
          dePage f = if takeExtension f == ".page"
                        then dropExtension f
                        else f

escape :: String -> String
escape = escapeURIString isAllowedInURI

formatFeedTime :: DateTime -> String
formatFeedTime = formatDateTime "%Y-%m%--%dT%TZ"  -- Why the double hyphen between %m and %d? It works.
                                                  -- A single hyphen seems to disappear - I don't know why!
