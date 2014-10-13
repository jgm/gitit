module Csv (plugin) where

-- TODO make 'file' paths relative to repository-path when absolute
-- TODO and relative to repository-path/cleaned-up-uri if relpaths

import Data.List
import Data.List.Split
import Data.Maybe
import Network.Gitit.Interface
import System.Directory
import System.FilePath.Posix

-- splits the raw csv into fields
-- TODO use Data.CSV to replace this
fields :: String -> [[String]]
fields s = map (splitOn ",") (lines s)

-- wraps a string in a tablecell
cell :: String -> TableCell
cell s = [Plain [Str s]]

align :: Int -> [Alignment]
align n = replicate n AlignDefault

table :: [Inline] -> String -> Block
table c t = Table c a w h r
  where
    f = map (map cell) $ fields t
    a = align $ length h
    w = [] -- relative widths

    -- TODO is this safe?
    h = head f
    r = tail f

-- extracts a caption from block attributes
caption :: [(String, String)] -> [Inline]
caption as = map Str $ maybeToList $ lookup "caption" as

uri2path :: String -> FilePath
uri2path uri
  = intercalate [pathSeparator]
  $ filter (/= "")                        -- remove blanks
  $ filter (\s -> not $ isPrefixOf "_" s) -- remove _edit etc.
  $ splitOn [pathSeparator] uri

-- returns the path to the .page file associated with a request
-- TODO should it find stuff in the ghc data dir too?
askFile :: PluginM FilePath
askFile = do
  cfg <- askConfig
  req <- askRequest
  let p1 = repositoryPath cfg
      p2 = uri2path $ rqUri req
      p  = joinPath [p1, p2]
  return p

-- takes an absolute or relative gitit link
-- and makes it into a file path relative to the running program
link2path :: String -> PluginM FilePath
link2path lnk = do
  cfg <- askConfig
  pfp <- askFile
  -- if it starts with "/", the link is relative to the repository
  -- otherwise, it's relative to the requested page
  return $ joinPath $ if "/" `isPrefixOf` lnk
    then [repositoryPath cfg, dropWhile (== '/') lnk]
    else [takeDirectory pfp, lnk]

-- gets block attributes and body text, and
-- reads a file to replace the text if needed
body :: [(String, FilePath)] -> String -> PluginM String
body as txt = case lookup "file" as of
  Nothing -> return txt
  Just f  -> do
    p <- link2path f
    e <- liftIO $ doesFileExist p
    case e of
      False -> return $ "file not found: " ++ p
      True  -> liftIO $ readFile p

plugin :: Plugin
plugin = mkPageTransformM tfm
  where
    tfm :: Block -> PluginM Block
    tfm (CodeBlock (_, cs, as) txt) | elem "csv" cs = do
      cap <- return $ caption as
      bod <- body as txt
      return $ table cap bod
    tfm x = return x
