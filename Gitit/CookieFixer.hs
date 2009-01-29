{-# OPTIONS_GHC -fno-warn-orphans #-}
{- From the HAppSHelpers package v. 0.10.
   (c) 2008 Thomas Hartman.
   Needed only until HAppS fixes cookie parsing.
-}

module Gitit.CookieFixer 
    ( cookieFixer
    ) where

import HAppS.Server.Cookie (Cookie(..))
import HAppS.Server(ServerPartT(..),Request(..), getHeader)

import qualified Data.ByteString.Char8 as C
import Data.Char (chr, toLower)
import Data.List ((\\))
import Data.Maybe

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide Parsec's definitions of some Applicative functions.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), token)

instance Applicative (GenParser s a) where
    pure = return
    (<*>) = ap

instance Alternative (GenParser s a) where
    empty = mzero
    (<|>) = mplus

parseCookiesM :: (Monad m) => String -> m [Cookie]
parseCookiesM str = either (fail "Invalid cookie syntax!") return $ parse cookiesParser str str

cookiesParser :: GenParser Char st [Cookie]
cookiesParser = av_pairs
    where -- Parsers based on RFC 2109
          av_pairs      = (:) <$> av_pair <*> many (char ';' *>  av_pair)
          av_pair       = cookie <$> attr <*> option "" (char '=' *> value)
          attr          = spaces *> token
          value         = word
          word          = incomp_token <|> quoted_string

          -- Parsers based on RFC 2068
          token         = many1 $ oneOf ((chars \\ ctl) \\ tspecials)
          quoted_string = char '"' *> many (oneOf qdtext) <* char '"'

          -- Custom parser, incompatible with RFC 2068, but very  forgiving ;)
          incomp_token  = many1 $ oneOf ((chars \\ ctl) \\ "\";")

          -- Primitives from RFC 2068
          tspecials     = "()<>@,;:\\\"/[]?={} \t"
          ctl           = map chr (127:[0..31])
          chars         = map chr [0..127]
          octet         = map chr [0..255]
          text          = octet \\ ctl
          qdtext        = text \\ "\""

cookie :: String -> String -> Cookie
cookie key value = Cookie "" "" "" (low key) value

cookieFixer :: ServerPartT m a -> ServerPartT m a
cookieFixer (ServerPartT sp) = ServerPartT $ \request -> sp (request { rqCookies = (fixedCookies request) } )
    where
      fixedCookies request = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" (rqHeaders request))), c <- cl ]

-- | Get all cookies from the HTTP request. The cookies are ordered per RFC from
-- the most specific to the least specific. Multiple cookies with the same
-- name are allowed to exist.
getCookies :: Monad m => C.ByteString -> m [Cookie]
getCookies header | C.null header = return []
                  | otherwise     = parseCookiesM (C.unpack header)

low :: String -> String
low = map toLower
