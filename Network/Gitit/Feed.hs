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

-- | Functions for creating Atom feeds for Gitit wikis and pages.

module Network.Gitit.Feed (FeedConfig(..), filestoreToXmlFeed) where

import Data.Time (UTCTime, formatTime, getCurrentTime, addUTCTime)
#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Foldable as F (concatMap)
import Data.List (intercalate, sortBy, nub)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Network.URI (isUnescapedInURI, escapeURIString)
import System.FilePath (dropExtension, takeExtension, (<.>))
import Data.FileStore.Types (history, Author(authorName), Change(..),
         FileStore, Revision(..), TimeRange(..))
import Text.Atom.Feed (nullEntry, nullFeed, nullLink, nullPerson,
         Date, Entry(..), Feed(..), Link(linkRel), Generator(..),
         Person(personName), TextContent(TextString))
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML.Light (ppTopElement)
import Data.Version (showVersion)
import Paths_gitit (version)

data FeedConfig = FeedConfig {
    fcTitle    :: String
  , fcBaseUrl  :: String
  , fcFeedDays :: Integer
 } deriving (Read, Show)

gititGenerator :: Generator
gititGenerator = Generator {genURI = Just "http://github.com/jgm/gitit"
                                   , genVersion = Just (showVersion version)
                                   , genText = "gitit"}

filestoreToXmlFeed :: FeedConfig -> FileStore -> Maybe FilePath -> IO String
filestoreToXmlFeed cfg f = fmap xmlFeedToString . generateFeed cfg gititGenerator f

xmlFeedToString :: Feed -> String
xmlFeedToString = ppTopElement . xmlFeed

generateFeed :: FeedConfig -> Generator -> FileStore -> Maybe FilePath -> IO Feed
generateFeed cfg generator fs mbPath = do
  now <- getCurrentTime
  revs <- changeLog (fcFeedDays cfg) fs mbPath now
  let home = fcBaseUrl cfg ++ "/"
  -- TODO: 'nub . sort' `persons` - but no Eq or Ord instances!
      persons = map authorToPerson $ nub $ sortBy (comparing authorName) $ map revAuthor revs
      basefeed = generateEmptyfeed generator (fcTitle cfg) home mbPath persons (formatFeedTime now)
      revisions = map (revisionToEntry home) revs
  return basefeed {feedEntries = revisions}

-- | Get the last N days history.
changeLog :: Integer -> FileStore -> Maybe FilePath -> UTCTime -> IO [Revision]
changeLog days a mbPath now' = do
  let files = F.concatMap (\f -> [f, f <.> "page"]) mbPath
  let startTime = addUTCTime (fromIntegral $ -60 * 60 * 24 * days) now'
  rs <- history a files TimeRange{timeFrom = Just startTime, timeTo = Just now'}
          (Just 200) -- hard limit of 200 to conserve resources
  return $ sortBy (flip $ comparing revDateTime) rs

generateEmptyfeed :: Generator -> String ->String ->Maybe String -> [Person] -> Date -> Feed
generateEmptyfeed generator title home mbPath authors now =
  baseNull {feedAuthors = authors,
            feedGenerator = Just generator,
            feedLinks = [ (nullLink $ home ++ "_feed/" ++ escape (fromMaybe "" mbPath))
                           {linkRel = Just (Left "self")}]
            }
    where baseNull = nullFeed home (TextString title) now

revisionToEntry :: String -> Revision -> Entry
revisionToEntry home Revision{ revId = rid, revDateTime = rdt,
                               revAuthor = ra, revDescription = rd,
                               revChanges = rv} =
  baseEntry{ entrySummary = Just $ TextString rd
           , entryAuthors = [authorToPerson ra], entryLinks = [ln] }
   where baseEntry = nullEntry url title (formatFeedTime rdt)
         url = home ++ escape (extract $ head rv) ++ "?revision=" ++ rid
         ln = (nullLink url) {linkRel = Just (Left "alternate")}
         title = TextString $ (takeWhile ('\n' /=) rd) ++ " - " ++ (intercalate ", " $ map show rv)

-- gitit is set up not to reveal registration emails
authorToPerson :: Author -> Person
authorToPerson ra = nullPerson {personName = authorName ra}

-- TODO: replace with Network.URI version of shortcut if it ever is added
escape :: String -> String
escape = escapeURIString isUnescapedInURI

formatFeedTime :: UTCTime -> String
formatFeedTime = formatTime defaultTimeLocale "%FT%TZ"

-- TODO: this boilerplate can be removed by changing Data.FileStore.Types to say
-- data Change = Modified {extract :: FilePath} | Deleted {extract :: FilePath} | Added
--                   {extract :: FilePath}
-- so then it would be just 'escape (extract $ head rv)' without the 4 line definition
extract :: Change -> FilePath
extract x = dePage $ case x of {Modified n -> n; Deleted n -> n; Added n -> n}
          where dePage f = if takeExtension f == ".page" then dropExtension f else f

-- TODO: figure out how to create diff links in a non-broken manner
{-
diff :: String -> String -> Revision -> Link
diff home path' Revision{revId = rid} =
                        let n = nullLink (home ++ "_diff/" ++ escape path' ++ "?to=" ++ rid) -- ++ fromrev)
                        in n {linkRel = Just (Left "alternate")}
-}
