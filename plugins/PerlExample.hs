module PerlExample (plugin) where

import Network.Gitit.Interface       (Plugin)
import Network.Gitit.Plugin.External (mkPlugin, allArgs)

plugin :: Plugin
plugin = mkPlugin
  "perlexample"    -- input block class
  "html"           -- output block class
  "perlexample.pl" -- name of script
  allArgs          -- list of args to pass
