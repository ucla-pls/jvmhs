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
  ClassPool ClassPreloader a
  -> IO (Either ClassPoolError a)
runTestClassPool =
  runClassPoolInClassPathOnly classpath

runTestClassPool' ::
  ClassPool ClassPreloader a
  -> IO a
runTestClassPool' scpt = do
  Right x <- runTestClassPool scpt
  return x

beforeClassPool ::
  ClassPool ClassPreloader a
  -> SpecWith a
  -> Spec
beforeClassPool scpt =
  before $ runTestClassPool' scpt
