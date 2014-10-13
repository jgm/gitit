module PythonExample (plugin) where

import Network.Gitit.Interface       (Plugin)
import Network.Gitit.Plugin.External (mkPlugin, allArgs)

plugin :: Plugin
plugin = mkPlugin
  "pythonexample"    -- input block class
  "html"             -- output block class
  "pythonexample.py" -- name of script
  allArgs            -- list of args to pass
