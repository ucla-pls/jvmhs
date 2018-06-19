{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.DeltaDebugTest where

import SpecHelper
import Jvmhs

import Data.Monoid

import Control.Monad.Writer.Class

import qualified Data.Vector as V
import qualified Data.IntSet as IS
import qualified Data.Set as S


spec_binarySearch :: Spec
spec_binarySearch = do
  it "can do a binary search on a list" $ do
    binarySearch (V.fromList [0..100])
      (\s -> tell [s] >> return (s >= 23))
      `shouldBe`
      ([50, 25, 12, 19, 22, 24, 23], (23 :: Int))


graph :: Graph Int ()
graph =
  mkGraphFromEdges
  [ (1, 2, ())
  , (2, 3, ())
  , (2, 4, ())
  ]

spec_gdd :: Spec
spec_gdd = do
  it "can find a minimal closure for k = 0" $ do
    gdd (\_ -> tell (Sum 1) >> return True) graph
      `shouldBe`
      (Sum (3 :: Int), [] :: [Int])
  it "can find a minimal closure for k = 1" $ do
    gdd (\s -> tell (Sum 1) >> return (any (== 2) s)) graph
      `shouldBe`
      (Sum (4 :: Int), [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $ do
    gdd(\s -> tell (Sum 1) >> return (any (== 3) s && any (== 4) s)) graph
      `shouldBe`
      (Sum (9 :: Int), [3, 4] :: [Int])

spec_sdd :: Spec
spec_sdd = do
  let v8 = V.fromList $ map (IS.singleton) [1..8]
  it "can return the empty set" $ do
    sdd (\_ -> tell (Sum 1) >> return True) v8
      `shouldBe` (Sum (4 :: Int), IS.fromList [])

  it "can solve a simple case " $ do
    sdd (\s -> tell (Sum 1) >> return (3 `IS.member` s)) v8
      `shouldBe` (Sum (4 :: Int), IS.fromList [3])

  it "can solve the dd-min case" $ do
    let
      s178 = IS.fromList [1,7,8]
      is178 s = return (s178 `IS.isSubsetOf` s)
    sdd (\s -> tell (Sum 1) >> is178 s) v8
      `shouldBe` (Sum (20 :: Int), IS.fromList [1,7,8])

  it "can solve a k=2 case " $ do
    let
      s167 = IS.fromList [1,6,7]
      s28 = IS.fromList [2,8]
      test s = return $ (s167 `IS.isSubsetOf` s) || (s28 `IS.isSubsetOf` s)
    sdd (\s -> tell (Sum 1) >> test s) v8
      `shouldBe` (Sum (28 :: Int), IS.fromList [2,8])


spec_ddmin :: Spec
spec_ddmin = do
  it "ddmin should be able to find elem with size 1" $
    do
      let numSet = S.fromList [1..8]
      rslt <- ddmin numSet is7
      S.toList rslt `shouldBe` [7]
  it "ddmin should be able to find [1,7,8]" $
      do
        let numSet = S.fromList [1..8]
        rslt <- ddmin numSet is178
        S.toList rslt `shouldBe` [1,7,8]
  where is7 v = return $ and [(S.member 7 v)]
        is178 v =
          return $ and [(S.member 1 v), (S.member 7 v), (S.member 8 v)]
