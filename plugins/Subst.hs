-- Usage:  a paragraph containing just [My page](!subst)
-- will be replaced by the contents of My page.
--
-- Limitations:  it is assumed that My page is
-- formatted with markdown, and contains no metadata.

module Subst (plugin) where

import Data.FileStore (retrieve)
import Text.Pandoc (defaultParserState, readMarkdown)
import Network.Gitit.ContentTransformer (inlinesToString)
import Network.Gitit.Interface
import Network.Gitit.Framework (filestoreFromConfig)

plugin :: Plugin
plugin = mkPageTransformM substituteIntoBlock

substituteIntoBlock :: [Block] -> PluginM [Block]
substituteIntoBlock (Para (Link ref ("!subst", _):_ ):xs) = 
     do let target = inlinesToString ref ++ ".page"
        cfg <- askConfig
        let fs = filestoreFromConfig cfg
        article <- liftIO (retrieve fs target Nothing)
        let (Pandoc _ content) = readMarkdown defaultParserState article
        (content ++) `fmap` substituteIntoBlock xs
substituteIntoBlock (x:xs) = (x:) `fmap` substituteIntoBlock xs
substituteIntoBlock [] = return []
