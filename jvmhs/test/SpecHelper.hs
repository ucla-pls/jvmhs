{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
  ( module Test.Tasty
  , module Test.Hspec.Expectations.Pretty
  , module Test.Tasty.QuickCheck
  , module Control.Lens

  , liftIO
  , it
  , SpecWith
  , Spec
  , before

  , classpath
  , runTestClassPool
  , runTestClassPool'
  , beforeClassPool
  ) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec.Expectations.Pretty
import Test.Tasty.QuickCheck
import Control.Monad.IO.Class

import Control.Lens hiding (elements)

import Jvmhs
-- import Jvmhs.ClassPool

classpath :: [ FilePath ]
classpath =
  [ "test/data/classes" ]


runTestClassPool ::
  ClassPool a
  -> IO ([ClassPoolReadError], a)
runTestClassPool a = do
  r <- preload $ fromClassPathOnly classpath
  (errs, st) <- loadClassPoolState r
  return $ (errs, fst $ runClassPool a st)

runTestClassPool' ::
  ClassPool a
  -> IO a
runTestClassPool' scpt = do
  ([], a) <- runTestClassPool scpt
  return a

beforeClassPool ::
  ClassPool a
  -> SpecWith a
  -> Spec
beforeClassPool scpt =
  before $ runTestClassPool' scpt
