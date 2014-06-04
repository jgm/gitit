{-# LANGUAGE PackageImports #-}

-- Usage:  a paragraph containing just [My page](!subst)
-- will be replaced by the contents of My page.
--
-- Limitations:  it is assumed that My page is
-- formatted with markdown, and contains no metadata.

module Subst (plugin) where

import "MonadCatchIO-mtl" Control.Monad.CatchIO (try)
import Data.FileStore (FileStoreError, retrieve)
import Text.Pandoc (def, readMarkdown)
import Network.Gitit.ContentTransformer (inlinesToString)
import Network.Gitit.Interface
import Network.Gitit.Framework (filestoreFromConfig)

plugin :: Plugin
plugin = mkPageTransformM substituteIntoBlock

substituteIntoBlock :: [Block] -> PluginM [Block]
substituteIntoBlock ((Para [Link ref ("!subst", _)]):xs) =
     do let target = inlinesToString ref
        cfg <- askConfig
        let fs = filestoreFromConfig cfg
        article <- try $ liftIO (retrieve fs (target ++ ".page") Nothing)
        case article :: Either FileStoreError String of
          Left  _    -> let txt = Str ("[" ++ target ++ "](!subst)")
                            alt = "'" ++ target ++ "' doesn't exist. Click here to create it."
                            lnk = Para [Link [txt] (target,alt)]
                        in  (lnk :) `fmap` substituteIntoBlock xs
          Right a    -> let (Pandoc _ content) = readMarkdown def a
                        in  (content ++) `fmap` substituteIntoBlock xs
substituteIntoBlock (x:xs) = (x:) `fmap` substituteIntoBlock xs
substituteIntoBlock [] = return []
