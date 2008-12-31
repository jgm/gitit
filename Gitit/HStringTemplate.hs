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

{- Replacements for HStringTemplate functions that don't handle
   UTF-8 properly. 
-}

module Gitit.HStringTemplate ( setAttribute ) 
where
import Codec.Binary.UTF8.String (encodeString)
import qualified Text.StringTemplate as T

-- | A wrapper around HStringTemplate's setAttribute that encodes strings
-- in UTF-8.
setAttribute :: String -> String -> T.StringTemplate String -> T.StringTemplate String
setAttribute attrName = T.setAttribute attrName . encodeString
