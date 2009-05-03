module SignaturePlugin (plugin) where

-- This plugin replaces $SIG$ with the username and timestamp
-- of the last edit, prior to saving the page in the repository.

import Gitit.Interface
import Data.Maybe (fromMaybe)
import Data.DateTime
import Control.Monad

plugin :: Plugin
plugin = PreCommitTransform replacedate

replacedate :: String -> PluginM String
replacedate [] = return ""
replacedate ('$':'S':'I':'G':'$':xs) = do
  datetime <- liftIO $ getCurrentTime
  mbuser <- askUser
  let username = case mbuser of
                   Nothing  -> "???"
                   Just u   -> uUsername u
  let sig = "-- " ++ username ++ " (" ++ formatDateTime "%c" datetime ++ ")"
  liftM (sig ++ ) $ replacedate xs
replacedate (x:xs) = liftM (x : ) $ replacedate xs

