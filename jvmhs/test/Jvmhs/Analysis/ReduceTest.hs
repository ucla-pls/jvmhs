{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.ReduceTest where

import SpecHelper
import Data.Graph.Inductive.Graph
import Jvmhs
import Jvmhs.Analysis.Reduce

outputPath :: FilePath
outputPath = "test/output/interface"


afterReduceInterface :: SpecWith () -> Spec
afterReduceInterface = before $ reduceInterface classpath outputPath "SimpleI"

spec_reduce :: Spec
spec_reduce = afterReduceInterface $ do
  let getInterfaces className = runTestClassPoolFromPath [outputPath] $ do
        cls <- loadClass className
        return (cls ^. classInterfaces)

  it "after removing interfaces, Itfc2 should replaced by ItfcParent" $ do
      x <- getInterfaces "SimpleI"
      x `shouldBe` ["ItfcParent", "Itfc"]


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



