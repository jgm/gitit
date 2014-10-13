module BashExample (plugin) where

import Network.Gitit.Interface       (Plugin)
import Network.Gitit.Plugin.External (mkPlugin, allArgs)

plugin :: Plugin
plugin = mkPlugin
  "bashexample"    -- input block class
  "html"           -- output block class
  "bashexample.sh" -- name of script
  allArgs          -- list of args to pass
