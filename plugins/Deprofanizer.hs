-- Deprofanizer.hs
module Deprofanizer (plugin) where

-- This plugin replaces profane words with "XXXXX".

import Gitit.Interface
import Data.Char (toLower)

plugin :: Plugin
plugin = mkPageTransform deprofanize

deprofanize :: Inline -> Inline
deprofanize (Str x) | isBadWord x = Str "XXXXX"
deprofanize x                     = x

isBadWord :: String -> Bool
isBadWord x = (map toLower x) `elem` ["darn", "blasted", "stinker"]
-- there are more, but this is a family program

