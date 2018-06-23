{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Jvmhs.Analysis.DeltaDebug
import Jvmhs.Data.Graph
import Control.Lens

import Data.List
--import qualified Data.Vector as V
import qualified Data.IntSet as IS

main :: IO ()
main =
  defaultMain
    [ simpleB
    , graphB
    ]

simpleB :: Benchmark
simpleB =
  bgroup "simple"
    [ mkP "one" (\i -> elem (i `quot` 2) ) [100,200,300]
    , mkP "two" (\i -> isSubsequenceOf [0, i - 1])  [50,100,150]
    , mkP "three" (\i -> isSubsequenceOf [0, i `quot` 2, i - 1]) [25,50,75]
    , mkP "half" (\i -> isSubsequenceOf [0..i `quot` 2]) [0.5,1,1.5]
    , mkP "even half" (\i -> isSubsequenceOf [0,2..i - 1]) [0.1,0.2..0.6]
    ]

  where
    mz = fromListOfSet :: [IS.IntSet] -> MZ

    runw p f i = runIdentity . f (pure . p i)

    mkP name p sizes =
      bgroup name
        [ mkSizes "ddmin" (runw p ddmin) sizes
        , mkSizes "sdd" (runw p $ sddx mz) sizes
        ]

    mkSizes name f sizes =
      bgroup name
        [ let n = floor (i * 1000) in
          bench (show i ++ "k") $ whnf (f n) ([0..n - 1] :: [Int])
        | i <- sizes :: [Float]
        ]

graphB :: Benchmark
graphB =
  bgroup "graph"
    [ runWith "ran-1000-1000" bench0
    , runWith "ran-1000-2000" bench0
    , runWith "ran-1000-3000" bench0
    , runWith "ran-1000-4000" bench0
    , runWith "ran-1000-5000" bench0
    , runWith "ran-2000-2000" bench0
    , runWith "ran-2000-4000" bench0
    , runWith "ran-2000-6000" bench0
    , runWith "ran-2000-8000" bench0
    , runWith "ran-2000-10000" bench0
    -- , runWith "ran-3000-3000" bench0
    ]

  where
    runWith grname f =
      env (graphFromFile ("benchmark/data/" ++ grname ++ ".txt")) (f grname)

    bench0 name gr =
      bench name $ whnf (runw (elem 0) gdd) gr

    runw p f = runIdentity . f (pure . p)
