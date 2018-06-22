{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Jvmhs.Analysis.DeltaDebug
import Control.Lens

import Data.List
import qualified Data.Vector as V

main :: IO ()
main =
  defaultMain
  [ mkP "one" (\i -> elem (i `quot` 2) ) [100,200,300]
  , mkP "two" (\i -> isSubsequenceOf [0, i - 1])  [100,200,300]
  , mkP "three" (\i -> isSubsequenceOf [0, i `quot` 2, i - 1]) [50,150,300]
  , mkP "half" (\i -> isSubsequenceOf [0..i `quot` 2]) [0.3,0.6,0.9]
  , mkP "even half" (\i -> isSubsequenceOf [0,2..i - 1]) [0.3,0.6,0.9]
  ]

mkP :: String -> (Int -> ([Int] -> Bool)) -> [Float] -> Benchmark
mkP name p sizes =
  bgroup name
    [ mkSizes "ddmin" (\i -> runIdentity . ddmin (pure . p i)) sizes
    , mkSizes "sdd" (\i -> runIdentity . sdd (pure . p i)) sizes
    ]

mkSizes :: String -> (Int -> [Int] -> [Int]) -> [Float] -> Benchmark
mkSizes name f sizes =
  bgroup name
    [
      let n = floor (i * 1000) in
      bench (show i ++ "k") $ whnf (f n) [0..n - 1]
    | i <- sizes
    ]
