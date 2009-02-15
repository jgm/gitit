module DotPlugin (plugin) where
import Gitit.Interface

import Text.Pandoc.Shared
import System.Process (readProcess)
import Data.Char (ord)
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA
import System.FilePath
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = Plugin {
    description = "This plugin allows you to include a graphviz 'dot' diagram\n" ++ 
                  "in a document like this:\n\n" ++
                  "~~~ {.dot name=\"diagram1\"}\n" ++
                  "digraph G {Hello->World}\n" ++
                  "~~~"
  , transformation = dotTransform }

dotTransform :: AppState -> Pandoc -> Web Pandoc
dotTransform st = processWithM (transformBlock st)

transformBlock :: AppState -> Block -> Web Block
transformBlock st (CodeBlock (id, classes, namevals) contents) | "dot" `elem` classes = do
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".png")
                                Nothing   -> ([], uniqueName contents ++ ".png")
  result <- liftIO $ readProcess "dot" ["-Tpng"] contents
  liftIO $ writeFile (staticDir (config st) </> "img" </> outfile) result
  return $ Para [Image name ("/img" </> outfile, "")]
transformBlock _ x = return x

-- | Generate a unique filename given the file's contents.
uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
