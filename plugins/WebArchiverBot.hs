{-| Scans page of Markdown looking for http links; when found it prints them out to a default file.
This plugin is meant to be run in conjunction with archiver <http://hackage.haskell.org/package/archiver>.
If you do not wish to run it (for example, you have no more than a dozen external http links on any page),
then you should use the original WebArchiver.hs plugin.

Limitations:
* Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
is hardwired into it.

By: Gwern Branwen; placed in the public domain -}

module WebArchiverBot (plugin) where

import System.Directory (getHomeDirectory)
import Network.Gitit.Interface (liftIO, bottomUpM, Plugin(PreCommitTransform), Inline(Link))
import Text.Pandoc (defaultParserState, readMarkdown)

plugin :: Plugin
plugin = PreCommitTransform archivePage

-- archivePage :: (MonadIO m) => String -> m String
archivePage x = do let p = readMarkdown defaultParserState x
                   -- force evaluation and archiving side-effects
                   _p' <- liftIO $ bottomUpM archiveLinks p
                   return x -- note: this is read-only - don't actually change page!

archiveLinks :: Inline -> IO Inline
archiveLinks x@(Link _ ('!':_, _)) = return x -- skip interwiki links
archiveLinks x@(Link _ ('#':_, _)) = return x -- skip section links
archiveLinks x@(Link _ (uln, _)) = do homedir <- getHomeDirectory
                                      appendFile (homedir++"/.urls.txt") (uln++"\n")
                                      return x
archiveLinks x                   = return x
