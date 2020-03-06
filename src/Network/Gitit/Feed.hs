{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
import Data.FileStore.Generic (Diff, PolyDiff(..), diff)
import Data.FileStore.Types (history, retrieve, Author(authorName), Change(..),
         FileStore, Revision(..), TimeRange(..), RevisionId)
import Text.Atom.Feed (nullEntry, nullFeed, nullLink, nullPerson,
         Date, Entry(..), Feed(..), Link(linkRel), Generator(..),
         Person(personName), EntryContent(..), TextContent(TextString))
import Text.Atom.Feed.Export (xmlFeed)
import Text.XML.Light as XML (showContent, Content(..), Element(..), blank_element, QName(..), blank_name, CData(..), blank_cdata)
import Text.XML as Text.XML (renderText, Document(..), Element(..),
                             Prologue(..), def, fromXMLElement)
import qualified Data.XML.Types as XMLTypes
import Data.Version (showVersion)
import Paths_gitit (version)

data FeedConfig = FeedConfig {
    fcTitle    :: String
  , fcBaseUrl  :: String
  , fcFeedDays :: Integer
 } deriving (Read, Show)

gititGenerator :: Generator
gititGenerator = Generator {genURI = Just "http://github.com/jgm/gitit"
                                   , genVersion = Just (T.pack (showVersion version))
                                   , genText = "gitit"}

filestoreToXmlFeed :: FeedConfig -> FileStore -> Maybe FilePath -> IO String
filestoreToXmlFeed cfg f = fmap xmlFeedToString . generateFeed cfg gititGenerator f

xmlFeedToString :: Feed -> String
xmlFeedToString elt = TL.unpack . renderText def $
  Document{ documentPrologue = Prologue{ prologueBefore = []
                                       , prologueDoctype = Nothing
                                       , prologueAfter = [] }
          , documentRoot = either (const $ Text.XML.Element "feed" mempty []) id
                            $ fromXMLElement $ xmlFeed elt
          , documentEpilogue = [] }

generateFeed :: FeedConfig -> Generator -> FileStore -> Maybe FilePath -> IO Feed
generateFeed cfg generator fs mbPath = do
  now <- getCurrentTime
  revs <- changeLog (fcFeedDays cfg) fs mbPath now
  diffs <- mapM (getDiffs fs) revs
  let home = fcBaseUrl cfg ++ "/"
  -- TODO: 'nub . sort' `persons` - but no Eq or Ord instances!
      persons = map authorToPerson $ nub $ sortBy (comparing authorName) $ map revAuthor revs
      basefeed = generateEmptyfeed generator (fcTitle cfg) home mbPath persons (T.pack (formatFeedTime now))
      revisions = map (revisionToEntry home) (zip revs diffs)
  return basefeed {feedEntries = revisions}

-- | Get the last N days history.
changeLog :: Integer -> FileStore -> Maybe FilePath -> UTCTime -> IO [Revision]
changeLog days a mbPath now' = do
  let files = F.concatMap (\f -> [f, f <.> "page"]) mbPath
  let startTime = addUTCTime (fromIntegral $ -60 * 60 * 24 * days) now'
  rs <- history a files TimeRange{timeFrom = Just startTime, timeTo = Just now'}
          (Just 200) -- hard limit of 200 to conserve resources
  return $ sortBy (flip $ comparing revDateTime) rs

getDiffs :: FileStore -> Revision -> IO [(FilePath, [Diff [String]])]
getDiffs fs Revision{ revId = to, revDateTime = rd, revChanges = rv } = do
  revPair <- history fs [] (TimeRange Nothing $ Just rd) (Just 2)
  let from = if length revPair >= 2
                then Just $ revId $ revPair !! 1
                else Nothing
  diffs <- mapM (getDiff fs from (Just to)) rv
  return $ map filterPages $ zip (map getFP rv) diffs
  where getFP (Added fp) = fp
        getFP (Modified fp) = fp
        getFP (Deleted fp) = fp
        filterPages (fp, d) = case (reverse fp) of
                                   'e':'g':'a':'p':'.':x -> (reverse x, d)
                                   _ -> (fp, [])

getDiff :: FileStore -> Maybe RevisionId -> Maybe RevisionId -> Change -> IO [Diff [String]]
getDiff fs from _ (Deleted fp) = do
  contents <- retrieve fs fp from
  return [First $ lines contents]
getDiff fs from to (Modified fp) = diff fs fp from to
getDiff fs _ to (Added fp) = do
  contents <- retrieve fs fp to
  return [Second $ lines contents]

generateEmptyfeed :: Generator -> String ->String ->Maybe String -> [Person] -> Date -> Feed
generateEmptyfeed generator title home mbPath authors now =
  baseNull {feedAuthors = authors,
            feedGenerator = Just generator,
            feedLinks = [ (nullLink $ T.pack $
                            home ++ "_feed/" ++ escape (fromMaybe "" mbPath))
                           {linkRel = Just (Left "self")}]
            }
    where baseNull = nullFeed (T.pack home) (TextString (T.pack title)) now

-- Several of the following functions have been given an alternate
-- version patched in using CPP specifically when the `feed` library
-- used is version 1.2.x
--
-- The 1.2.0.0 release of `feed` introduced an API change that was
-- reverted in 1.3.0.0.  When feed-1.2.x is sufficiently out of use,
-- the dependency lower-bound for feed should be updated to >= 1.3 and
-- the second alternative (#else condition) in each of the CPP
-- invocations below can be removed.
--
-- https://github.com/bergmark/feed/issues/35

revisionToEntry :: String -> (Revision, [(FilePath, [Diff [String]])]) -> Entry
revisionToEntry home (Revision{ revId = rid, revDateTime = rdt,
                               revAuthor = ra, revDescription = rd,
                               revChanges = rv}, diffs) =
  baseEntry{ entryContent = Just $ HTMLContent content
           , entryAuthors = [authorToPerson ra], entryLinks = [ln] }
   where baseEntry = nullEntry (T.pack url) title
                          (T.pack $ formatFeedTime rdt)
         url = home ++ escape (extract $ head rv) ++ "?revision=" ++ rid
         ln = (nullLink (T.pack url)) {linkRel = Just (Left "alternate")}
         title = TextString $ T.pack $ (takeWhile ('\n' /=) rd) ++ " - " ++ (intercalate ", " $ map show rv)
         df = map diffFile diffs
#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
         content = T.pack $ concat $ map showContent df
#else
         content = XMLTypes.Element (XMLTypes.Name "div" Nothing Nothing) [] df
#endif

#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
diffFile :: (FilePath, [Diff [String]]) -> Content
diffFile (fp, d) =
    enTag "div" $ header : text
  where
    header = enTag1 "h1" $ enText fp
    text = map (enTag1 "p") $ concat $ map diffLines d
#else
diffFile :: (FilePath, [Diff [String]]) -> XMLTypes.Node
diffFile (fp, d) =
    enTag "div" $ header : text
  where
    header = enTag1 "h1" $ enText $ T.pack fp
    text = map (enTag1 "p") $ concat $ map diffLines d
#endif

#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
diffLines :: Diff [String] -> [Content]
diffLines (First x) = map (enTag1 "s" . enText) x
diffLines (Second x) = map (enTag1 "b" . enText) x
diffLines (Both x _) = map enText x
#else
diffLines :: Diff [String] -> [XMLTypes.Node]
diffLines (First x) = map (enTag1 "s" . enText . T.pack) x
diffLines (Second x) = map (enTag1 "b" . enText . T.pack) x
diffLines (Both x _) = map (enText . T.pack) x
#endif

#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
enTag :: String -> [Content] -> Content
enTag tag content = Elem blank_element{ elName=blank_name{qName=tag}
                                      , elContent=content
                                      }
#else
enTag :: T.Text -> [XMLTypes.Node] -> XMLTypes.Node
enTag tag content = XMLTypes.NodeElement $
  XMLTypes.Element
  (XMLTypes.Name tag Nothing Nothing)
  [] content
#endif

#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
enTag1 :: String -> Content -> Content
#else
enTag1 :: T.Text -> XMLTypes.Node -> XMLTypes.Node
#endif
enTag1 tag content = enTag tag [content]

#if MIN_VERSION_feed(1, 3, 0) || (! MIN_VERSION_feed(1, 2, 0))
enText :: String -> Content
enText content = Text blank_cdata{cdData=content}
#else
enText :: T.Text -> XMLTypes.Node
enText content = XMLTypes.NodeContent (XMLTypes.ContentText content)
#endif

-- gitit is set up not to reveal registration emails
authorToPerson :: Author -> Person
authorToPerson ra = nullPerson {personName = T.pack $ authorName ra}

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
