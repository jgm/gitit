{-# LANGUAGE CPP #-}
module Network.Gitit.Compat.Except (
                                   ExceptT
                                 , Except
                                 , Error(..)
                                 , runExceptT
                                 , runExcept
                                 , MonadError
                                 , throwError
                                 , catchError )
       where

import Control.Monad.Except

class Error a where
  noMsg  :: a
  strMsg :: String -> a

  noMsg    = strMsg ""
  strMsg _ = noMsg

