module MTable (plugin) where

-- This plugin ideally parses a less irritating table syntax
-- for gitit.
-- 
-- ~~~ {.mtable}
-- | foo | bar | bop |
-- | and | so  | on  |
-- ~~~
-- 
-- Unfortuantely it still can't do colspan
--
-- Simon Heath <icefoxen@gmail.com> 2013

import Data.Default
import Data.List.Split
import Text.Pandoc (readMarkdown)
import Network.Gitit.Interface
import System.IO.Unsafe

parseTableBlock :: Block -> PluginM Block
parseTableBlock (CodeBlock (_, classes, namevals) contents) | "mtable" `elem` classes = do
  let lineses = lines contents
      splittedLines = map (splitOn "|") lineses
      splittedLinesWithEmptiesRemoved = map (filter ((/=) "")) splittedLines
      parseCell x = 
        let Pandoc _ blk = readMarkdown def x in
            blk
      cellify line = map (\cell -> parseCell cell) line
      cells = map cellify splittedLinesWithEmptiesRemoved
      alignments = replicate (length (head cells)) AlignDefault
  return $ Table [] alignments [] [] cells --(head cells) (tail cells)

parseTableBlock x = return x

plugin :: Plugin
plugin = mkPageTransformM parseTableBlock
