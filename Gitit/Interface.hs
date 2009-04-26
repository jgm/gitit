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

{- | Interface for plugins.

A plugin is a Haskell module that is dynamically loaded by gitit.
The only kind of plugin currently defined is the 'PageTransform'
plugin, which modifies the 'Pandoc' document that results after
a page's markdown source is parsed, but before it is converted
to HTML:

>                     +-----------------+
>                     | markdown source |
>                     +-----------------+
>                              ||         <----  markdown reader
>                              \/
>                     +-----------------+
>                     | Pandoc document |
>                     +-----------------+
>                              ||         <---- PageTransform plugins
>                              \/
>                   +---------------------+
>                   | new Pandoc document |
>                   +---------------------+
>                              ||         <---- HTML writer
>                              \/
>                   +----------------------+
>                   | HTML version of page |
>                   +----------------------+

'PageTransform' plugins do not alter the page source stored in
the repository.  They only affect what is visible on the website.

You can use the helper functions 'mkPageTransform' and 'mkPageTransformM'
to create 'PageTransform' plugins from a transformation of any
of the basic types used by Pandoc (for example, 'Inline', 'Block',
'[Inline]', even 'String'). Here is a simple (if silly) example:

> -- DeprofanizerPlugin.hs
> module DeprofanizerPlugin (plugin) where
> 
> -- This plugin replaces profane words with "XXXXX".
> 
> import Gitit.Interface
> import Data.Char (toLower)
> 
> plugin :: Plugin
> plugin = mkPageTransform deprofanize
> 
> deprofanize :: Inline -> Inline
> deprofanize (Str x) | isBadWord x = Str "XXXXX"
> deprofanize x                     = x
> 
> isBadWord :: String -> Bool
> isBadWord x = (map toLower x) `elem` ["darn", "blasted", "stinker"]
> -- there are more, but this is a family program

-}

module Gitit.Interface ( getConfig
                       , Config(..)
                       , AppState(..)
                       , User(..)
                       , FileStore(..)
                       , getFileStore
                       , Web
                       , look
                       , module Text.Pandoc.Definition
                       , Plugin(..)
                       , inlinesToURL
                       , inlinesToString
                       , mkPageTransform
                       , mkPageTransformM
                       )
where
import Text.Pandoc.Definition
import Data.FileStore
import Data.Data
import Gitit.State
import Gitit.Server
import Gitit.ContentTransformer

-- | Lifts a function from @a -> a@ (for example, @Inline -> Inline@,
-- @Block -> Block@, @[Inline] -> [Inline]@, or @String -> String@)
-- to a 'PageTransform' plugin.
mkPageTransform :: Data a => (a -> a) -> Plugin
mkPageTransform fn = PageTransform $ \st doc ->
                      do updateAppState (const st)
                         return $ processWith fn doc

-- | Monadic version of 'mkPageTransform'.
-- Lifts a function from @a -> Web a@ to a 'PageTransform' plugin.
mkPageTransformM :: Data a => (a -> Web a) -> Plugin
mkPageTransformM fn =  PageTransform $ \st doc ->
                        do updateAppState (const st)
                           processWithM fn doc

