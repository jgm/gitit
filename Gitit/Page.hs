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

module Gitit.Page
  ( pageAsPandoc
  , rawContents
  ) where

import Control.Exception (throwIO, catch)
import Control.Monad.Trans (liftIO)
import Data.FileStore
import Gitit.Convert
import Gitit.Framework
import Gitit.Server
import Gitit.State
import Prelude hiding (catch)
import Text.Pandoc

rawContents :: String -> Params -> Web (Maybe String)
rawContents file params = do
  let rev = pRevision params
  fs <- getFileStore
  liftIO $ catch (retrieve fs file rev >>= return . Just) (\e -> if e == NotFound then return Nothing else throwIO e)

pageAsPandoc :: String -> Params -> Web (Maybe Pandoc)
pageAsPandoc page params = do
  pt <- getDefaultPageType
  mDoc <- rawContents (pathForPage page) params
  case mDoc of
        Nothing -> return Nothing
        Just d  -> do
          (Pandoc _ blocks) <- textToPandoc pt page d
          return $ Just $ Pandoc (Meta [Str page] [] []) blocks
