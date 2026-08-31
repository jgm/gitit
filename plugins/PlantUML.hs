module PlantUML (plugin) where

-- This plugin allows you to include a plantuml diagram
-- in a page like this:
--
-- ~~~ {.puml}
-- @startuml
-- cloud cloud1
-- cloud cloud2
-- cloud cloud3
-- cloud cloud4
-- cloud cloud5
-- cloud1 -0- cloud2
-- cloud1 -0)- cloud3
-- cloud1 -(0- cloud4
-- cloud1 -(0)- cloud5
-- @enduml
-- ~~~
--
-- The "dot" and "plantuml" executable must be in the path.
-- The generated png file will be saved in the static img directory.

import GHC.IO.Handle
import Network.Gitit.Interface
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
-- from the temporary package on HackageDB
import System.IO.Temp (withTempFile)
-- from the utf8-string package on HackageDB:
import Data.ByteString.Lazy.UTF8 (fromString)

import System.Environment (unsetEnv)
import System.FilePath ((</>))
import System.FilePath

plugin :: Plugin
plugin = mkPageTransformM transformBlock

transformBlock :: Block -> PluginM Block
transformBlock (CodeBlock (id, classes, namevals) contents) | "puml" `elem` classes = do
  cfg <- askConfig
  let filetype = "svg"
      outdir = staticDir cfg </> "img"
  liftIO $ withTempFile outdir "diag.puml" $ \infile inhandle -> do
    unsetEnv "DISPLAY"
    hPutStr inhandle contents
    hClose inhandle
    (ec, stdout, stderr) <- readProcessWithExitCode "plantuml"
      [ "-t" ++ filetype
      , infile
      ] ""
    let outname = takeFileName $ infile -<.> filetype
    if ec == ExitSuccess
       then return $ Para [ Image (id,classes,namevals) [Str outname] ("/img" </> outname, "") ]
       else error $ "plantuml returned an error status: " ++ stderr
transformBlock x = return x
