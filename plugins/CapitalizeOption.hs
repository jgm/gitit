module CapitalizeOption (plugin) where

-- This plugin capitalizes all strings on a page if the
-- 'capitalize' metadata field is set to 'yes' or 'true'.
-- This shows how to get access to metadata in a plugin.

import Network.Gitit.Interface
import Data.Char (toUpper)

plugin :: Plugin
plugin = PageTransform $ \doc -> do
  meta <- askMeta
  case lookup "capitalize" meta of
       Just s | map toUpper s `elem` ["YES","TRUE"] ->
         return $ processWith capStr doc
       _ -> return doc

capStr :: Inline -> Inline
capStr (Str x) = Str (map toUpper x)
capStr x       = x
