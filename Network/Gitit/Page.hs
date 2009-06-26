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

{- Functions for translating between Page structures and raw
-  text strings.
-}

module Network.Gitit.Page ( stringToPage
                          , pageToString
                          )
where
import Network.Gitit.Types

-- | Read a string (the contents of a page file) and produce a Page
-- object, using defaults except when overridden by metadata.
stringToPage = undefined

-- | Write a string (the contents of a page file) corresponding to
-- a Page object, using explicit metadata only when needed.
pageToString = undefined

