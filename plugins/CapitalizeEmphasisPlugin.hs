module CapitalizeEmphasisPlugin (plugin) where

-- This plugin converts emphasized text to ALL CAPS.
-- Not a very useful feature, but useful as an example
-- of how to write a plugin.

import Gitit.Interface
import Data.Char (toUpper)
import Data.Generics (everywhereM, mkM, everywhere, mkT)

plugin :: Plugin
plugin = PageTransform transform

transform :: AppState -> String -> Pandoc -> Web Pandoc
transform _ _ = everywhereM (mkM (return . capsTransform))

capsTransform :: [Inline] -> [Inline]
capsTransform (Emph x : ys) = everywhere (mkT capStr) x ++ capsTransform ys
capsTransform (x : ys)      = x : capsTransform ys
capsTransform []            = []

capStr :: Inline -> Inline
capStr (Str x) = Str (map toUpper x)
capStr x       = x
