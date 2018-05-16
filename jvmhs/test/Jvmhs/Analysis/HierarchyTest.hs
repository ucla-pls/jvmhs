{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.HierarchyTest where

import SpecHelper

import Data.Graph.Inductive.Graph

import Jvmhs

unit_Calculate_simple_hierarchy :: IO ()
unit_Calculate_simple_hierarchy = do
  e <- runTestClassPool $ do
    x <- calculateHierarchy ["Extended", "Simple"]

    liftIO $ prettyPrint (x^.hryExtends)

  e `shouldBe` Right ()
