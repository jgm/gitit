-- | Scans page of Markdown looking for http links. When it finds them, it submits them
-- to webcitation.org / https://secure.wikimedia.org/wikipedia/en/wiki/WebCite
--
-- Limitations:
-- * Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
-- is hardwired into it.
--
-- By: Gwern Branwen; placed in the public domain

module WebArchiver (plugin) where

import Network.Gitit.Interface (askUser, liftIO, processWithM, uEmail, Plugin(PreCommitTransform), Inline(Link))
import Control.Monad (when)
import Network.URI (isURI)
import Control.Concurrent (forkIO)
import Network.HTTP (getRequest, simpleHTTP)
import Text.Pandoc (defaultParserState, readMarkdown)
import Control.Monad.Trans (MonadIO)

plugin :: Plugin
plugin = PreCommitTransform archivePage

-- archivePage :: (MonadIO m) => String -> ReaderT (Config, Maybe User) (StateT IO) String
archivePage x = do mbUser <- askUser
                   let email = case mbUser of
                        Nothing -> "nobody@mailinator.com"
                        Just u  -> uEmail u
                   let p = readMarkdown defaultParserState x
                   -- force evaluation and archiving side-effects
                   _p' <- liftIO $ processWithM (archiveLinks email) p
                   return x -- note: this is read-only - don't actually change page!

archiveLinks :: String -> Inline -> IO Inline
archiveLinks e x@(Link _ (uln, _)) = checkArchive e uln >> return x
archiveLinks _ x = return x

-- | Error check the URL.
checkArchive :: (MonadIO m) => String -> String -> m ()
checkArchive e u = when (isURI u) (liftIO $ archiveURL e u)

archiveURL :: String -> String -> IO ()
archiveURL eml url = forkIO (openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ eml) >> return ()) >> return ()
   where openURL = simpleHTTP . getRequest
