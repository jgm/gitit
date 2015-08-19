{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- Re-exports Happstack functions needed by gitit, including
   replacements for Happstack functions that don't handle UTF-8 properly, and
   new functions for setting headers and zipping contents and for looking up IP
   addresses.
-}

module Network.Gitit.Server
          ( module Happstack.Server
          , withExpiresHeaders
          , setContentType
          , setFilename
          , lookupIPAddr
          , getHost
          , compressedResponseFilter
          )
where
import Happstack.Server
import Happstack.Server.Compression (compressedResponseFilter)
import Network.Socket (getAddrInfo, defaultHints, addrAddress)
import Control.Monad.Reader
import Data.ByteString.UTF8 as U hiding (lines)

withExpiresHeaders :: ServerMonad m => m Response -> m Response
withExpiresHeaders = liftM (setHeader "Cache-Control" "max-age=21600")

setContentType :: String -> Response -> Response
setContentType = setHeader "Content-Type"

setFilename :: String -> Response -> Response
setFilename = setHeader "Content-Disposition" . \fname -> "attachment; filename=\"" ++ fname ++ "\""

-- IP lookup

lookupIPAddr :: String -> IO (Maybe String)
lookupIPAddr hostname = do
  addrs <- getAddrInfo (Just defaultHints) (Just hostname) Nothing
  if null addrs
     then return Nothing
     else return $ Just $ takeWhile (/=':') $ show $ addrAddress $ case addrs of -- head addrs
                                                                     [] -> error "lookupIPAddr, no addrs"
                                                                     (x:_) -> x
getHost :: ServerMonad m => m (Maybe String)
getHost = liftM (maybe Nothing (Just . U.toString)) $ getHeaderM "Host"
