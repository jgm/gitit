{-
Copyright (C) 2009 John MacFarlane <jgm@berkeley.edu>
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

{- General framework for defining wiki actions. 
-}

module Gitit.Framework (
                         Handler
                       , filterIf
                       , gzipBinary
                       , acceptsZip
                       , withExpiresHeaders
                       , setContentType
                       , setFilename
                       )
where
import HAppS.Server hiding (look, lookRead, lookCookieValue, mkCookie)
import Data.DateTime
import Control.Monad (liftM) 
import qualified Data.Map as M
import Codec.Compression.GZip (compress)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe (isJust)

type Handler = ServerPart Response

filterIf :: (Request -> Bool) -> (Response -> Response) -> ServerPart Response -> ServerPart Response
filterIf test filt sp =
  let handler = unServerPartT sp
  in  withRequest $ \req ->
      if test req
         then liftM filt $ handler req
         else handler req

gzipBinary :: Response -> Response
gzipBinary r@(Response {rsBody = b}) =  setHeader "Content-Encoding" "gzip" $ r {rsBody = compress b}

acceptsZip :: Request -> Bool
acceptsZip req = isJust $ M.lookup (fromString "accept-encoding") (rqHeaders req)

getCacheTime :: IO (Maybe DateTime)
getCacheTime = liftM (Just . addMinutes 360) $ getCurrentTime

withExpiresHeaders :: ServerPart Response -> ServerPart Response
withExpiresHeaders sp = require getCacheTime $ \t -> [liftM (setHeader "Expires" $ formatDateTime "%a, %d %b %Y %T GMT" t) sp]

setContentType :: String -> Response -> Response
setContentType = setHeader "Content-Type"

setFilename :: String -> Response -> Response
setFilename = setHeader "Content-Disposition" . \fname -> "attachment: filename=\"" ++ fname ++ "\""


