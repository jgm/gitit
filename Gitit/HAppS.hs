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

{- Replacements for HAppS functions that don't handle UTF-8 properly.
-}

module Gitit.HAppS
           ( look
           , lookRead
           , lookCookieValue
           , mkCookie
           )
where
import HAppS.Server hiding (look, lookRead, lookCookieValue, mkCookie)
import qualified HAppS.Server (lookCookieValue, mkCookie)
import Text.Pandoc.CharacterReferences (decodeCharacterReferences)
import Control.Monad (liftM)
import Data.ByteString.Lazy.UTF8 (toString)
import Codec.Binary.UTF8.String (encodeString, decodeString)

-- HAppS's look, lookRead, and lookCookieValue encode unicode characters
-- (outside the standard latin1 range) using decimal character
-- references. For gitit's purposes, we want them to return regular
-- unicode characters instead.

look :: String -> RqData String
look = liftM (decodeCharacterReferences . toString) . HAppS.Server.lookBS

lookRead :: Read a => String -> RqData a
lookRead = liftM read . look

lookCookieValue :: String -> RqData String
lookCookieValue = liftM decodeString . HAppS.Server.lookCookieValue

mkCookie :: String -> String -> Cookie
mkCookie name val = HAppS.Server.mkCookie name (encodeString val)

