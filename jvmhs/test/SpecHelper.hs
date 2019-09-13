{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
  ( module Test.Hspec.Expectations.Pretty
  , module Test.Hspec.QuickCheck
  , module Control.Lens

  , liftIO
  , it
  , SpecWith
  , Spec
  , before
  , describe

  , classpath
  , runTestClassPool
  , runTestClassPool'
  , beforeClassPool
  , beforeClass
  , forEveryClassIt

  , useOutputFolder
  ) where

import Text.Printf

import Control.Monad

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Expectations.Pretty
import Control.Monad.IO.Class

import System.Directory
import System.IO.Error

import Data.Maybe

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
  (errs, st) <- loadClassPoolState (defaultFromReader r)
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

beforeClass ::
  ClassName
  -> SpecWith Class
  -> Spec
beforeClass cn = beforeClassPool $ fromJust <$> getClass cn

forEveryClassIt ::
  (HasCallStack, Example a)
  => String
  -> (Class -> a)
  -> SpecWith (Arg a)
forEveryClassIt n fn = do
  _classes <- runIO $ runTestClassPool' allClasses
  forM_ _classes $ \cls ->
    it (printf "%s (%s)" n (show (cls ^. className))) $ fn cls


useOutputFolder ::
  String
  -> SpecWith a
  -> SpecWith a
useOutputFolder folder = beforeAll_ $ do
  _ <- tryIOError $ removeDirectoryRecursive folder
  createDirectoryIfMissing True folder
