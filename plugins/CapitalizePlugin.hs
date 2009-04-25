module CapitalizePlugin (plugin) where

-- This plugin converts text to ALL CAPS.

import Gitit.Interface
import Data.Char (toUpper)

plugin :: Plugin
plugin = PageTransform transform

transform :: Pandoc -> Web Pandoc
transform = return . processWith toUpper
