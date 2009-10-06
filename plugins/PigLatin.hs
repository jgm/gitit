module PigLatin (plugin) where

-- This plugin converts a page to pig latin if the 'language' metadata
-- field is set to 'pig latin'. This demonstrates how to get access to
-- metadata in a plugin.

import Network.Gitit.Interface
import Data.Char (toLower, toUpper, isLower)

plugin :: Plugin
plugin = PageTransform $ \doc -> do
  meta <- askMeta
  case lookup "language" meta of
       Just s | map toLower s == "pig latin" ->
         return $ processWith pigLatinStr doc
       _ -> return doc

pigLatinStr :: Inline -> Inline
pigLatinStr (Str "") = Str ""
pigLatinStr (Str x) | isLower (head x) && head x `notElem` "aeiou" =
  Str (tail x ++ (head x : "ay"))
pigLatinStr (Str x) | toLower (head x) `notElem` "aeiou" =
  Str (capitalize (tail x) ++ (toLower (head x) : "ay"))
pigLatinStr (Str x) = Str (x ++ "yay")
pigLatinStr x       = x

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
