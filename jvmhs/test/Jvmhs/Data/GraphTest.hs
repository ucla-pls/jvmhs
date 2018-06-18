{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Data.GraphTest where

import SpecHelper
import Jvmhs

import Data.Monoid

import Control.Monad.Writer.Class

import qualified Data.Vector as V


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

spec_partition :: Spec
spec_partition = do
  it "can handle the simple graph" $ do
    partition graph `shouldBe` [ ([4], [4]), ([3], [3]), ([2], [2, 3, 4]), ([1], [1, 2, 3, 4]) ]

spec_gdd :: Spec
spec_gdd = do
  it "can find a minimal closure for k = 0" $ do
    gdd graph (\_ -> tell (Sum 1) >> return True)
      `shouldBe`
      (Sum (3 :: Int), [] :: [Int])
  it "can find a minimal closure for k = 1" $ do
    gdd graph (\s -> tell (Sum 1) >> return (any (== 2) s))
      `shouldBe`
      (Sum (3 :: Int), [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $ do
    gdd graph (\s -> tell (Sum 1) >> return (any (== 3) s && any (== 4) s))
      `shouldBe`
      (Sum (3 :: Int), [3, 4] :: [Int])
