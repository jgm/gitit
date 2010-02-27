module CapitalizeEmphasis (plugin) where

-- This plugin converts emphasized text to ALL CAPS.
-- Not a very useful feature, but useful as an example
-- of how to write a plugin.

import Network.Gitit.Interface
import Data.Char (toUpper)

plugin :: Plugin
plugin = mkPageTransform capsTransform

capsTransform :: [Inline] -> [Inline]
capsTransform (Emph x : xs) = processWith capStr x ++ capsTransform xs
capsTransform (x:xs)        = x : capsTransform xs
capsTransform []            = []

capStr :: Inline -> Inline
capStr (Str x) = Str (map toUpper x)
capStr x       = x
