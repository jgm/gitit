{-| Scans page of Markdown looking for http links. When it finds them, it submits them
to webcitation.org / https://secure.wikimedia.org/wikipedia/en/wiki/WebCite
(It will also submit them to Alexa (the source for the Internet Archive), but Alexa says that
its bots take weeks to visit and may not ever.)

This module employs the archiver daemon <http://hackage.haskell.org/package/archiver> as a library; `cabal install archiver` will install it.

Limitations:
* Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
is hardwired into it.
* No rate limitation or choking; will fire off all requests as fast as possible.
  If pages have more than 20 external links or so, this may result in your IP being temporarily
  banned by WebCite. To avoid this, you can use WebArchiverBot.hs instead, which will parse & dump
  URLs into a file processed by the archiver daemon (which *is* rate-limited).

By: Gwern Branwen; placed in the public domain -}

module WebArchiver (plugin) where

import Control.Concurrent (forkIO)
import Network.URL.Archiver as A (checkArchive)
import Network.Gitit.Interface (askUser, bottomUpM, liftIO, uEmail, Plugin(PreCommitTransform), Inline(Link))
import Text.Pandoc (defaultParserState, readMarkdown)

plugin :: Plugin
plugin = PreCommitTransform archivePage

-- archivePage :: String -> ReaderT PluginData (StateT Context IO) String
archivePage x = do mbUser <- askUser
                   let email = case mbUser of
                        Nothing -> "nobody@mailinator.com"
                        Just u  -> uEmail u
                   let p = readMarkdown defaultParserState x
                   -- force evaluation and archiving side-effects
                   _p' <- liftIO $ bottomUpM (archiveLinks email) p
                   return x -- note: this is read-only - don't actually change page!

archiveLinks :: String -> Inline -> IO Inline
archiveLinks e x@(Link _ (uln, _)) = forkIO (A.checkArchive e uln) >> return x
archiveLinks _ x                   = return x
