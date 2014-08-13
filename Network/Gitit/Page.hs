{-# LANGUAGE CPP #-}
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

{- Functions for translating between Page structures and raw
-  text strings.  The strings may begin with a metadata block,
-  which looks like this (it is valid YAML):
-
-  > ---
-  > title: Custom Title
-  > format: markdown+lhs
-  > toc: yes
-  > categories: foo bar baz
-  > ...
-
-  This would tell gitit to use "Custom Title" as the displayed
-  page title (instead of the page name), to interpret the page
-  text as markdown with literate haskell, to include a table of
-  contents, and to include the page in the categories foo, bar,
-  and baz.
-
-  The metadata block may be omitted entirely, and any particular line
-  may be omitted. The categories in the @categories@ field should be
-  separated by spaces. Commas will be treated as spaces.
-
-  Metadata value fields may be continued on the next line, as long as
-  it is nonblank and starts with a space character.
-
-  Unrecognized metadata fields are simply ignored.
-}

module Network.Gitit.Page ( stringToPage
                          , pageToString
                          , readCategories
                          )
where
import Network.Gitit.Types
import Network.Gitit.Util (trim, splitCategories, parsePageType)
import Text.ParserCombinators.Parsec
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO (withFile, Handle, IOMode(..))
import qualified Control.Exception as E
import System.IO.Error (isEOFError)
#if MIN_VERSION_base(4,5,0)
#else
import Codec.Binary.UTF8.String (encodeString)
#endif

parseMetadata :: String -> ([(String, String)], String)
parseMetadata raw =
  case parse pMetadataBlock "" raw of
    Left _           -> ([], raw)
    Right (ls, rest) -> (ls, rest)

pMetadataBlock :: GenParser Char st ([(String, String)], String)
pMetadataBlock = try $ do
  _ <- string "---"
  _ <- pBlankline
  ls <- many pMetadataLine
  _ <- string "..."
  _ <- pBlankline
  skipMany pBlankline
  rest <- getInput
  return (ls, rest)

pBlankline :: GenParser Char st Char
pBlankline = try $ many (oneOf " \t") >> newline

pMetadataLine :: GenParser Char st (String, String)
pMetadataLine = try $ do
  first <- letter
  rest <- many (letter <|> digit <|> oneOf "-_")
  let ident = first:rest
  skipMany (oneOf " \t")
  _ <- char ':'
  rawval <- many $ noneOf "\n\r"
                 <|> (try $ newline >> notFollowedBy pBlankline >>
                            skipMany1 (oneOf " \t") >> return ' ')
  _ <- newline
  return (ident, trim rawval)

-- | Read a string (the contents of a page file) and produce a Page
-- object, using defaults except when overridden by metadata.
stringToPage :: Config -> String -> String -> Page
stringToPage conf pagename raw =
  let (ls, rest) = parseMetadata raw
      page' = Page { pageName        = pagename
                   , pageFormat      = defaultPageType conf
                   , pageLHS         = defaultLHS conf
                   , pageTOC         = tableOfContents conf
                   , pageTitle       = pagename
                   , pageCategories  = []
                   , pageText        = filter (/= '\r') rest
                   , pageMeta        = ls }
  in  foldr adjustPage page' ls

adjustPage :: (String, String) -> Page -> Page
adjustPage ("title", val) page' = page' { pageTitle = val }
adjustPage ("format", val) page' = page' { pageFormat = pt, pageLHS = lhs }
    where (pt, lhs) = parsePageType val
adjustPage ("toc", val) page' = page' {
  pageTOC = map toLower val `elem` ["yes","true"] }
adjustPage ("categories", val) page' =
   page' { pageCategories = splitCategories val ++ pageCategories page' }
adjustPage (_, _) page' = page'

-- | Write a string (the contents of a page file) corresponding to
-- a Page object, using explicit metadata only when needed.
pageToString :: Config -> Page -> String
pageToString conf page' =
  let pagename   = pageName page'
      pagetitle  = pageTitle page'
      pageformat = pageFormat page'
      pagelhs    = pageLHS page'
      pagetoc    = pageTOC page'
      pagecats   = pageCategories page'
      metadata   = filter
                       (\(k, _) -> not (k `elem`
                           ["title", "format", "toc", "categories"]))
                       (pageMeta page')
      metadata'  = (if pagename /= pagetitle
                       then "title: " ++ pagetitle ++ "\n"
                       else "") ++
                   (if pageformat /= defaultPageType conf ||
                          pagelhs /= defaultLHS conf
                       then "format: " ++
                            map toLower (show pageformat) ++
                            if pagelhs then "+lhs\n" else "\n"
                       else "") ++
                   (if pagetoc /= tableOfContents conf
                       then "toc: " ++
                            (if pagetoc then "yes" else "no") ++ "\n"
                       else "") ++
                   (if not (null pagecats)
                       then "categories: " ++ intercalate ", " pagecats ++ "\n"
                       else "") ++
                   (unlines (map (\(k, v) -> k ++ ": " ++ v) metadata))
  in (if null metadata' then "" else "---\n" ++ metadata' ++ "...\n\n")
        ++ pageText page'

-- | Read categories from metadata strictly.
readCategories :: FilePath -> IO [String]
readCategories f =
#if MIN_VERSION_base(4,5,0)
  withFile f ReadMode $ \h ->
#else
  withFile (encodeString f) ReadMode $ \h ->
#endif
    E.catch (do fl <- B.hGetLine h
                if dashline fl
                   then do -- get rest of metadata
                     rest <- hGetLinesTill h dotline
                     let (md,_) = parseMetadata $ unlines $ "---":rest
                     return $ splitCategories $ fromMaybe ""
                            $ lookup "categories" md
                   else return [])
       (\e -> if isEOFError e then return [] else E.throwIO e)

dashline :: B.ByteString -> Bool
dashline x =
  case BC.unpack x of
       ('-':'-':'-':xs) | all (==' ') xs -> True
       _ -> False

dotline :: B.ByteString -> Bool
dotline x =
  case BC.unpack x of
       ('.':'.':'.':xs) | all (==' ') xs -> True
       _ -> False

hGetLinesTill :: Handle -> (B.ByteString -> Bool) -> IO [String]
hGetLinesTill h end = do
  next <- B.hGetLine h
  if end next
     then return [toString next]
     else do
       rest <- hGetLinesTill h end
       return (toString next:rest)
