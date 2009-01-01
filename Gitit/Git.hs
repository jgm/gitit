{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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

{- Auxiliary functions for running git commands.

   Note:  UTF-8 locale is assumed.
-}

module Gitit.Git
           ( runGitCommand
           , gitLastCommitHash
           , gitLog
           , gitLsTree
           , gitGrep
           , gitCatFile
           , gitDiff
           , gitCommit
           , gitRemove
           , gitGetSHA1
           , gitMergeFile
           , LogEntry (..) )
where

import Control.Monad (unless, liftM)
import Control.Monad.Trans
import Network.CGI (urlEncode)
import System.Exit
import qualified Text.ParserCombinators.Parsec as P
import Prelude hiding (readFile, writeFile)
import Codec.Binary.UTF8.String (decodeString)
import HAppS.State
import Gitit.Shell (runProgCommand)
import Gitit.State
import Data.Char (chr)

-- | Run git command and return error status, standard output, and error output.  The repository
-- is used as working directory.
runGitCommand :: MonadIO m => String -> [String] -> m (ExitCode, String, String)
runGitCommand command args = do
  repo <- liftM repositoryPath (query GetConfig)
  let env = Just [("GIT_DIFF_OPTS","-u100000")]
  liftIO $ runProgCommand repo env "git" command args

-- | Return SHA1 hash of last commit for filename.
gitLastCommitHash :: MonadIO m => String -> m (Maybe String)
gitLastCommitHash filename = do
  (status, _, output) <- runGitCommand "log" $ ["--pretty=format:%H", "--"] ++ [filename]
  let outputWords = words output
  if status == ExitSuccess && not (null outputWords)
     then return $ Just $ head outputWords
     else return Nothing

-- | Return list of log entries for the given time frame and commit author.
-- If author is null, return entries for all authors.
gitLog :: MonadIO m => String -> String -> [String] -> m [LogEntry]
gitLog since author files = do
  (status, err, output) <- runGitCommand "whatchanged" $ ["--pretty=format:%h%n%cr%n%an%n%s%n"] ++
                                                         ["--since='" ++ urlEncode since ++ "'"] ++
                                                         (if null author then [] else ["--author=" ++ author]) ++
                                                         ["--"] ++ files
  if status == ExitSuccess
     then case P.parse parseGitLog "" output of
                Left err'    -> error $ show err'
                Right parsed -> return parsed
     else error $ "git whatchanged returned error status.\n" ++ err

gitLsTree :: MonadIO m => String -> m [String]
gitLsTree rev = do
  (status, errOutput, output) <- runGitCommand "ls-tree" ["-r", rev]
  if status == ExitSuccess
     then return $ map (convertEncoded . (unwords . drop 3 . words)) $ lines output
     else error $ "git ls-tree returned error status.\n" ++ errOutput

-- | git ls-tree returns UTF-8 filenames in quotes, with characters octal-escaped.
-- like this: "\340\244\226.page"
-- This function decodes these.
convertEncoded :: String -> String
convertEncoded s =
  case P.parse pEncodedString s s of
    Left _    -> s
    Right res -> res

pEncodedString :: P.GenParser Char st [Char]
pEncodedString = do
  P.char '"'
  res <- P.many1 (pOctalChar P.<|> P.anyChar)
  if last res == '"'
     then return $ decodeString $ init res
     else fail "No ending quotation mark."

pOctalChar :: P.GenParser Char st Char
pOctalChar = P.try $ do
  P.char '\\'
  ds <- P.count 3 (P.oneOf "01234567")
  let num = read $ "0o" ++ ds
  return $ chr num

gitGrep :: MonadIO m => [String] -> m String
gitGrep patterns = do
  (status, errOutput, output) <- runGitCommand "grep" (["--all-match", "--ignore-case", "--word-regexp"] ++
                                   concatMap (\term -> ["-e", term]) patterns)
  if status == ExitSuccess
     then return output
     else error $ "git grep returned error status.\n" ++ errOutput

gitCatFile :: MonadIO m => String -> FilePath -> m (Maybe String)
gitCatFile revision file = do
  (status, _, output) <- runGitCommand "cat-file" ["-p", revision ++ ":" ++ file]
  return $ if status == ExitSuccess
              then Just output
              else Nothing

gitDiff :: MonadIO m
        => String     -- ^ Filename
        -> String     -- ^ Old version (sha1)
        -> String     -- ^ New version (sha1)
        -> m String  -- ^ String
gitDiff file from to = do
  (status, _, output) <- runGitCommand "diff" [from, to, file]
  if status == ExitSuccess
     then return output
     else do
       -- try it without the path, since the error might be "not in working tree" for a deleted file
       (status', errOut', output') <- runGitCommand "diff" [from, to]
       if status' == ExitSuccess
          then return output'
          else error $ "git diff returned error: " ++ errOut'

-- | Add and then commit file, raising errors if either step fails.
gitCommit :: MonadIO m => FilePath -> (String, String) -> String -> m ()
gitCommit file (author, email) logMsg = do
  (statusAdd, errAdd, _) <- runGitCommand "add" [file]
  if statusAdd == ExitSuccess
     then do (statusCommit, errCommit, _) <- runGitCommand "commit" ["--author", author ++ " <" ++
                                               email ++ ">", "-m", logMsg]
             if statusCommit == ExitSuccess
                then return ()
                else unless (null errCommit) $ error $ "Could not git commit " ++ file ++ "\n" ++ errCommit
     else error $ "Could not git add " ++ file ++ "\n" ++ errAdd

-- | Remove file from repository and commit, raising errors if either step fails.
gitRemove :: MonadIO m => FilePath -> (String, String) -> String -> m ()
gitRemove file (author, email) logMsg = do
  (statusAdd, errAdd, _) <- runGitCommand "rm" [file]
  if statusAdd == ExitSuccess
     then do (statusCommit, errCommit, _) <- runGitCommand "commit" ["--author", author ++ " <" ++
                                               email ++ ">", "-m", logMsg]
             if statusCommit == ExitSuccess
                then return ()
                else unless (null errCommit) $ error $ "Could not git commit " ++ file ++ "\n" ++ errCommit
     else error $ "Could not git rm " ++ file ++ "\n" ++ errAdd

gitGetSHA1 :: MonadIO m => FilePath -> m (Maybe String)
gitGetSHA1 file = do
  (status, _, out) <- runGitCommand "log" ["-n", "1", "--pretty=oneline", file]
  if status == ExitSuccess && length out > 0
     then return $ Just $ head $ words out
     else return $ Nothing

gitMergeFile :: MonadIO m => FilePath -> FilePath -> FilePath -> m String
gitMergeFile edited original latest = do
  (status, err, out) <- runGitCommand "merge-file" ["--stdout", edited, original, latest]
  case status of
       ExitSuccess             -> return out
       ExitFailure n | n >= 0  -> return out  -- indicates number of merge conflicts
       _                       -> error $ "git merge-file returned an error.\n" ++ err

--
-- Parsers to parse git log into LogEntry records.
--

-- | Abstract representation of a git log entry.
data LogEntry = LogEntry
  { logRevision :: String
  , logDate :: String
  , logAuthor :: String
  , logSubject :: String
  , logFiles :: [String]
  } deriving (Read, Show)

parseGitLog :: P.Parser [LogEntry]
parseGitLog = P.manyTill gitLogEntry P.eof

wholeLine :: P.GenParser Char st [Char]
wholeLine = P.manyTill P.anyChar P.newline

nonblankLine :: P.GenParser Char st [Char]
nonblankLine = P.notFollowedBy P.newline >> wholeLine

gitLogEntry :: P.Parser LogEntry
gitLogEntry = do
  rev <- nonblankLine
  date <- nonblankLine
  author <- wholeLine
  subject <- liftM unlines (P.manyTill wholeLine (P.eof P.<|> (P.lookAhead (P.char ':') >> return ())))
  P.spaces
  files <- P.many gitLogChange
  P.spaces
  return $ LogEntry { logRevision = rev,
                      logDate = date,
                      logAuthor = author,
                      logSubject = subject,
                      logFiles = map convertEncoded files }

gitLogChange :: P.Parser String
gitLogChange = do
  P.char ':'
  line <- nonblankLine
  return $ unwords $ drop 5 $ words line
