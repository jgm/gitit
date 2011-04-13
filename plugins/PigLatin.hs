module PigLatin (plugin) where

-- This plugin converts a page to pig latin if the 'language' metadata
-- field is set to 'pig latin'. This demonstrates how to get access to
-- metadata in a plugin.

import Network.Gitit.Interface
import Data.Char (toLower, toUpper, isLower, isUpper, isLetter)

plugin :: Plugin
plugin = PageTransform $ \doc -> do
  meta <- askMeta
  case lookup "language" meta of
       Just s | map toLower s == "pig latin" ->
         return $ processWith pigLatinStr doc
       _ -> return doc

pigLatinStr :: Inline -> Inline
pigLatinStr (Str "") = Str ""
pigLatinStr (Str (c:cs)) | isLower c && isConsonant c =
  Str (cs ++ (c : "ay"))
pigLatinStr (Str (c:cs)) | isUpper c && isConsonant c =
  Str (capitalize cs ++ (toLower c : "ay"))
pigLatinStr (Str x@(c:_)) | isLetter c = Str (x ++ "yay")
pigLatinStr x       = x

isConsonant :: Char -> Bool
isConsonant c = c `notElem` "aeiouAEIOU"

capitalize :: String -> String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs
