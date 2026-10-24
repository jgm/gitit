{-# LANGUAGE LambdaCase #-}
import System.Directory
import System.FilePath
import System.Process
import Data.List
import System.Environment
import GHC.Stack
import Control.Monad
import System.IO.Unsafe

mustBeOne :: (HasCallStack, Show a) => [a] -> a
mustBeOne = \case 
    [one] -> one
    [] -> error "Expected one element, got 0"
    other -> error $ "Expected one element, got " ++ show (length other) ++ ": " ++ show other

main = do
    packages <- getArgs
    dir <- getEnv "DIR"
    forM_ packages $ \package -> do
        dataDir <- drop (length "data-dir: ") . head . lines <$> readProcess "stack" ["exec", "--", "ghc-pkg", "field", package, "data-dir"] ""
        let targetDir = dir </> "vendor-data"
        createDirectoryIfMissing True targetDir
        callProcess "cp" ["-R", dataDir, targetDir </> package]
