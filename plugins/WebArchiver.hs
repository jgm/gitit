{-| Scans page of Markdown looking for http links. When it finds them, it submits them
to webcitation.org / https://secure.wikimedia.org/wikipedia/en/wiki/WebCite
(It will also submit them to Alexa (the source for the Internet Archive), but Alexa says that
its bots take weeks to visit and may not ever.)

Limitations:
* Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
is hardwired into it.

By: Gwern Branwen; placed in the public domain -}

module WebArchiver (plugin) where

import Control.Concurrent (forkIO)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO)
import Data.Maybe (fromJust)
import Network.Browser (browse, formToRequest, request, Form(..))
import Network.HTTP (getRequest, rspBody, simpleHTTP, RequestMethod(POST))
import Network.URI (isURI, parseURI, uriPath)

import Network.Gitit.Interface (askUser, liftIO, processWithM, uEmail, Plugin(PreCommitTransform), Inline(Link))
import Text.Pandoc (defaultParserState, readMarkdown)

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
archiveLinks e x@(Link _ (uln, _)) = forkIO (checkArchive e uln) >> return x
archiveLinks _ x                   = return x

-- | Error check the URL and then archive it both ways
checkArchive :: (MonadIO m) => String -> String -> m ()
checkArchive email url = when (isURI url) $ liftIO (webciteArchive email url >> alexaArchive url)

webciteArchive :: String -> String -> IO ()
webciteArchive email url = ignore $ openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=" ++ email)
   where openURL = simpleHTTP . getRequest
         ignore = fmap $ const ()

alexaArchive :: String -> IO ()
alexaArchive url = do let archiveform = Form POST
                             (fromJust $ parseURI "http://www.alexa.com/help/crawlrequest")
                                 [("url", url), ("submit", "")]
                      (uri, resp) <- browse $ request $ formToRequest archiveform
                      when (uriPath uri /= "/help/crawlthanks") $
                           error $ "Request failed! Did Alexa change webpages? Response:" ++ rspBody resp
