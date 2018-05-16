{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
  ( module Test.Tasty
  , module Test.Hspec.Expectations.Pretty
  , module Test.Tasty.QuickCheck
  , module Control.Lens

  , liftIO

  , classpath
  , runTestClassPool
  ) where

import Test.Tasty
import Test.Hspec.Expectations.Pretty
import Test.Tasty.QuickCheck
import Control.Monad.IO.Class

import Control.Lens hiding (elements)

import Jvmhs
-- import Jvmhs.ClassPool

classpath :: [ FilePath ]
classpath = [ "test/data/classes" ]

runTestClassPool :: ClassPool ClassPreloader a -> IO (Either ClassPoolError a)
runTestClassPool = runClassPoolInClassPathOnly classpath
