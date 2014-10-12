module Dot (plugin) where

-- This plugin allows you to include a graphviz dot diagram
-- in a page like this:
--
-- ~~~ {.dot name="diagram1"}
-- digraph G {Hello->World}
-- ~~~
--
-- The "dot" executable must be in the path.
-- The generated svg file will be cached in the cache directory.
-- A unique name will be generated from a hash of the file contents,
-- prefixed with the 'name' attribute if one is given.

import Data.Maybe (fromMaybe)
import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "dot" `elem` classes = do
  cfg <- askConfig
  let prefix  = fromMaybe "dot" $ lookup "name" namevals
      outfile = cacheDir cfg </> prefix ++ "-" ++ uniqueName contents ++ ".svg"
      dotargs = ["-Tsvg", "-o", outfile]
  cached <- liftIO $ doesFileExist outfile
  unless cached $ do
    (ec, _out, err) <- liftIO $ readProcessWithExitCode "dot" dotargs contents
    -- TODO fix so it doesn't crash the wiki with an error!
    unless (ec == ExitSuccess) $ error $ "dot returned an error status: " ++ err
  svg <- liftIO $ readFile outfile
  return $ RawBlock (Format "html") svg
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
