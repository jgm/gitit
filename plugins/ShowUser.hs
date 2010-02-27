module ShowUser (plugin) where

-- This plugin replaces $USER$ with the name of the currently logged in
-- user, or the empty string if no one is logged in.

import Network.Gitit.Interface

plugin :: Plugin
plugin = mkPageTransformM showuser

showuser :: Inline -> PluginM Inline
showuser (Math InlineMath x) | x == "USER"  = do
  doNotCache  -- tell gitit not to cache this page, as it has dynamic content
  mbUser <- askUser
  case mbUser of
       Nothing -> return $ Str ""
       Just u  -> return $ Str $ uUsername u
showuser x = return x

