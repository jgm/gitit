module DotPlugin (plugin) where

-- This plugin allows you to include a graphviz dot diagram
-- in a page like this:
--
-- ~~~ {.dot name="diagram1"}
-- digraph G {Hello->World}
-- ~~~
--
-- The "dot" executable must be in the path.
-- The generated png file will be saved in the static img directory.
-- If no name is specified, a unique name will be generated from a hash
-- of the file contents.

import Network.Gitit.Interface
import System.Process
import System.Exit
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA
import System.FilePath
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "dot" `elem` classes = do
  cfg <- askConfig
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".png")
                                Nothing   -> ([], uniqueName contents ++ ".png")
  (ec, out, err) <- liftIO $ readProcessWithExitCode "dot" ["-Tpng"] contents
  if ec == ExitSuccess
     then do
       liftIO $ writeFile (staticDir cfg </> "img" </> outfile) out
       return $ Para [Image name ("/_static/img" </> outfile, "")]
     else error $ "dot returned an error status: " ++ err
transformBlock x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
