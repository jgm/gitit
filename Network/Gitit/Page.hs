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
-  which looks like this:
-
-  > !title: Custom Title 
-  > !format: markdown+lhs
-  > !toc: yes
-  > !categories: foo bar baz
-
-  This would tell gitit to use "Custom Title" as the displayed
-  page title (instead of the page name), to interpret the page
-  text as markdown with literate haskell, to include a table of
-  contents, and to include the page in the categories foo, bar,
-  and baz.
- 
-  The metadata block must end with a blank line.  It may be
-  omitted entirely, and any particular line may be omitted.
-  The categories in the @categories@ field should be separated
-  by spaces.  Commas will be treated as spaces.
-  
-  Metadata value fields may be continued on the next line, as
-  long as it is nonblank and starts with a space character.
-
-  Unrecognized metadata fields are simply ignored.
-}

module Network.Gitit.Page ( stringToPage
                          , pageToString
                          )
where
import Network.Gitit.Types
import Network.Gitit.Config (parsePageType)
import Network.Gitit.Util (trim, splitCategories)
import Text.ParserCombinators.Parsec
import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (intercalate)

parseMetadata :: String -> ([(String, String)], String)
parseMetadata raw =
  case parse pMetadataBlock "" raw of
    Left err         -> error $ "Error parsing page: " ++ show err
    Right (ls, rest) -> (ls, rest)

pMetadataBlock :: GenParser Char st ([(String, String)], String)
pMetadataBlock = do
  ls <- many pMetadataLine
  unless (null ls) $ pBlankline >> return ()
  rest <- getInput
  return (ls, rest)

pBlankline :: GenParser Char st Char
pBlankline = try $ many (oneOf " \t") >> newline

pMetadataLine :: GenParser Char st (String, String)
pMetadataLine = try $ do
  char '!'
  ident <- many1 letter
  char ':'
  rawval <- many $ noneOf "\n\r"
                 <|> (try $ newline >> notFollowedBy pBlankline >>
                            skipMany1 (oneOf " \t") >> return ' ')
  newline
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
                   , pageText        = filter (/= '\r') rest }
  in  foldr adjustPage page' ls

adjustPage :: (String, String) -> Page -> Page
adjustPage ("title", val) page' = page' { pageTitle = val }
adjustPage ("format", val) page' = page' { pageFormat = pt, pageLHS = lhs }
    where (pt, lhs) = parsePageType val
adjustPage ("toc", val) page' = page' {
  pageTOC = (map toLower val) `elem` ["yes","true"] }
adjustPage ("categories", val) page' =
   page' { pageCategories = splitCategories val }
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
      metadata'  = (if pagename /= pagetitle
                       then "!title: " ++ pagetitle ++ "\n"
                       else "") ++
                   (if pageformat /= defaultPageType conf ||
                          pagelhs /= defaultLHS conf
                       then "!format: " ++ 
                            map toLower (show pageformat) ++
                            if pagelhs then "+lhs\n" else "\n"
                       else "") ++
                   (if pagetoc /= tableOfContents conf
                       then "!toc: " ++
                            (if pagetoc then "yes" else "no") ++ "\n"
                       else "") ++
                   (if not (null pagecats)
                       then "!categories: " ++ intercalate " " pagecats ++ "\n"
                       else "")
  in  metadata' ++ (if null metadata' then "" else "\n") ++ pageText page'

