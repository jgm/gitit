module Signature (plugin) where

-- This plugin replaces $SIG$ with the username and timestamp
-- of the last edit, prior to saving the page in the repository.

import Network.Gitit.Interface
import Data.DateTime (getCurrentTime, formatDateTime)

plugin :: Plugin
plugin = PreCommitTransform replacedate

replacedate :: String -> PluginM String
replacedate [] = return ""
replacedate ('$':'S':'I':'G':'$':xs) = do
  datetime <- liftIO getCurrentTime
  mbuser <- askUser
  let username = case mbuser of
                   Nothing  -> "???"
                   Just u   -> uUsername u
  let sig = concat ["-- ", username, " (", formatDateTime "%c" datetime, ")"]
  fmap (sig ++ ) $ replacedate xs
replacedate (x:xs) = fmap (x : ) $ replacedate xs

