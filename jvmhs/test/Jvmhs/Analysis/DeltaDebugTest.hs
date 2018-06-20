{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.DeltaDebugTest where

import SpecHelper
import Jvmhs

import Data.Monoid

import Control.Monad.Writer.Class
import Control.Monad.Trans.Maybe
import Control.Monad

import qualified Data.Vector as V
import qualified Data.IntSet as IS


spec_binarySearch :: Spec
spec_binarySearch = do
  let l100 = (V.fromList ([0..100] :: [Int]))
  it "can do a binary search on a list" $ do
    binarySearch (\s -> tell [s] >> pure (s >= 23)) l100
      `shouldBe`
      ([50, 25, 12, 19, 22, 24, 23], Just 23)

  it "returns the smallest element if true" $ do
    binarySearch (\_ -> pure True) l100 `shouldBe` Identity (Just 0)

  it "returns Nothing if false" $ do
    binarySearch (\_ -> pure False) l100 `shouldBe` Identity Nothing


graph :: Graph Int ()
graph =
  mkGraphFromEdges
  [ (1, 2, ())
  , (2, 3, ())
  , (2, 4, ())
  ]

-- spec_gdd :: Spec
-- spec_gdd = do
--   it "can find a minimal closure for k = 0" $ do
--     gdd (\_ -> tell (Sum 1) >> return True) graph
--       `shouldBe`
--       (Sum (3 :: Int), Just ([] :: [Int]))
--   it "can find a minimal closure for k = 1" $ do
--     gdd (\s -> tell (Sum 1) >> return (any (== 2) s)) graph
--       `shouldBe`
--       (Sum (4 :: Int), Just ([2, 3, 4] :: [Int]))
--   it "can find a minimal closure for k = 2" $ do
--     gdd(\s -> tell (Sum 1) >> return (any (== 3) s && any (== 4) s)) graph
--       `shouldBe`
--       (Sum (9 :: Int), Just ([3, 4] :: [Int]))

-- spec_sdd :: Spec
-- spec_sdd = do
--   let v8 = V.fromList $ map (IS.singleton) [1..8]
--   -- it "can return the empty set" $ do
--   --   sdd (\_ -> tell (Sum 1) >> return True) v8
--   --     `shouldBe` (Sum (4 :: Int), IS.fromList [])

--   it "can solve a simple case " $ do
--     sdd' (\s -> tell (Sum 1) >> return (3 `IS.member` s)) v8
--       `shouldBe` (Sum (4 :: Int), IS.fromList [3])

--   it "can solve the dd-min case" $ do
--     let
--       s178 = IS.fromList [1,7,8]
--       is178 s = return (s178 `IS.isSubsetOf` s)
--     sdd' (\s -> tell (Sum 1) >> is178 s) v8
--       `shouldBe` (Sum (20 :: Int), IS.fromList [1,7,8])

--   it "can solve a k=2 case " $ do
--     let
--       s167 = IS.fromList [1,6,7]
--       s28 = IS.fromList [2,8]
--       test s = return $ (s167 `IS.isSubsetOf` s) || (s28 `IS.isSubsetOf` s)
--     sdd' (\s -> tell (Sum 1) >> test s) v8
--       `shouldBe` (Sum (28 :: Int), IS.fromList [2,8])


spec_ddmin :: Spec
spec_ddmin = do
  it "is be able to find elem with size 1" $
    ddmin (\a -> tell (Sum 1) >> is7 a) l8
      `shouldBe` (Sum (5:: Int), [7])
  it "is be able to find [1,7,8]" $
    ddmin (\a -> tell (Sum 1) >> is178 a) l8
      `shouldBe` (Sum (31:: Int), [1,7,8])
  it "returns a single element from the set if true" $
    ddmin (\_ -> tell (Sum 1) >> pure True) l8
      `shouldBe` (Sum (3:: Int), [1])
  it "returns everything if false" $
    ddmin (\_ -> tell (Sum 1) >> pure False) l8
      `shouldBe` (Sum (28 :: Int), l8)
  where
    l8 :: [Int]
    l8 = [1..8]
    is7 v = return $ elem (7 :: Int) v
    is178 v =
      return $ and [(elem 1 v), (elem 7 v), (elem (8 :: Int) v)]
