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

import Gitit.Interface
import Text.Pandoc.Shared
import System.Process (readProcess)
import Data.Char (ord)
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)
-- from the SHA package on HackageDB:
import Data.Digest.Pure.SHA
import Data.Generics (everywhereM, mkM)
import System.FilePath
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = PageTransform dotTransform

dotTransform :: AppState -> Pandoc -> Web Pandoc
dotTransform st = everywhereM (mkM (transformBlock st))

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
