{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.ReduceTest where

import SpecHelper
import Data.Graph.Inductive.Graph
import Jvmhs

spec_reduce :: SpecWith ()
spec_reduce = do
  it "should show up as a test" $ do
    1 `shouldBe` (1 :: Int)
  -- before rtcp $ do
  -- it "can find the subclasses of 'java.lang.Object'" $ \ hry -> do
  --   subclasses hry "java.lang.Object" `shouldBe` ["Simple", "Extended"]

  -- it "can find the superclasses of 'Extended'" $ \ hry -> do
  --   superclasses hry "Extended" `shouldBe` ["Simple", "java.lang.Object"]

  -- where
  --   rtcp = do
  --     Right hry <- runTestClassPool $
  --       calculateHierarchy ["Extended", "Simple"]
  --     return hry
