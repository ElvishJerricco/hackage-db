module Main (main) where

import Distribution.Hackage.DB.Path
import Distribution.Hackage.DB.Unparsed
import Distribution.Hackage.DB.Utility

import Control.Monad
import Data.Map as Map
import qualified Data.Vector as V
import Distribution.Package
import System.Environment

main :: IO ()
main = do
  packageIds <- getArgs
  db         <- hackageTarball >>= readTarball Nothing
  forM_ packageIds $ \pid -> do
    let PackageIdentifier pn v = parseText "PackageIdentifier" pid
    putStrLn $ maybe
      ("*** unknown package identifier on Hackage: " ++ show pid)
      (show . fmap fst . zip [(0::Int) ..] . V.toList . revisions)
      (Map.lookup pn db >>= Map.lookup v . versions)
