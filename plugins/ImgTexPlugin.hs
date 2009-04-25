module ImgTexPlugin (plugin) where
{-

This plugin provides a clear math LaTeX output.
(* latex and dvipng executable must be in the path.)

like this:

~~~ {.dvipng}
\nabla \times \bm{V}
=
\frac{1}{h_1 h_2 h_3}
  \begin{vmatrix}
    h_1 e_1 & h_2 e_2 & h_3 e_3 \\
    \frac{\partial}{\partial q_{1}} &
    \frac{\partial}{\partial q_{2}} &
    \frac{\partial}{\partial q_{3}} \\
    h_1 V_1 & h_2 V_2 & h_3 V_3
  \end{vmatrix}
~~~

License: GPL
written by Kohei OZAKI <i@smly.org>

-}

import Gitit.Interface
import Text.Pandoc.Shared
import System.Process (system)
import System.Directory
import Data.Char (ord)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA
import System.FilePath
import Control.Monad.Trans (liftIO)

plugin :: Plugin
plugin = mkPageTransformM transformBlock

tmpdir = "/var/tmp/"

templateHeader =
    ( "\\documentclass[12pt]{article}\n"
      ++ "\\usepackage{amsmath,amssymb,bm}\n"
      ++ "\\begin{document}\n"
      ++ "\\thispagestyle{empty}\n"
      ++ "\\[\n"
    )

templateFooter =
    ( "\n"
      ++ "\\]\n"
      ++ "\\end{document}\n"
    )

transformBlock :: Block -> Web Block
transformBlock (CodeBlock (id, classes, namevals) contents)
    | "dvipng" `elem` classes = do
  cfg <- getConfig
  let (name, outfile) =  case lookup "name" namevals of
                                Just fn   -> ([Str fn], fn ++ ".png")
                                Nothing   -> ([], uniqueName contents ++ ".png")
  liftIO $ do
    curr <- getCurrentDirectory
    initTempDir outfile
    writeFile (outfile++".tex") (templateHeader ++ contents ++ templateFooter)
    system $ "latex " ++ (outfile++".tex") ++ " > /dev/null"
    setCurrentDirectory curr
    system $ "dvipng -T tight -bd 1000 -freetype0 -Q 5 --gamma 1.3 "
           ++ tmpdir ++ "gitit-tmp-" ++ outfile ++ "/" ++ outfile ++ ".dvi"
           ++ " -o " ++ (staticDir cfg </> "img" </> outfile)
           ++ " > /dev/null"
    finishTempDir outfile
  return $ Para [Image name ("/img" </> outfile, "")]
transformBlock x = return x

mkTempDirName :: String -> String
mkTempDirName = \s -> tmpdir ++ "gitit-tmp-" ++ s

initTempDir :: String -> IO ()
initTempDir = (\f -> createDirectory f >> setCurrentDirectory f) . mkTempDirName

finishTempDir :: String -> IO ()
finishTempDir = removeDirectoryRecursive . mkTempDirName

uniqueName :: String -> String
uniqueName = showDigest . sha1 . fromString
