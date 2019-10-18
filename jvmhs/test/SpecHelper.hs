{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module SpecHelper
  ( module Test.Hspec.Expectations.Pretty
  , module Test.Hspec.QuickCheck
  , module Control.Lens

  , liftIO
  , it
  , describe
  , fdescribe
  , xdescribe
  , SpecWith
  , Spec
  , before
  , runIO

  , beforeMethod

  , classpath
  , runTestClassPool
  , runTestClassPool'
  , beforeClassPool
  , beforeClass
  , forEveryClassIt


  , runJREClassPool
  , withJREClassMethods
  , getJREHierachy
  , withJREMethodIt

  , useOutputFolder
  , getClassFromTestPool
  ) where

import Text.Printf
import Control.Monad
import Control.Exception

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Expectations.Pretty
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BL

-- aeson
import Data.Aeson

import System.Directory
import System.IO.Error

import Data.Maybe

import Control.Lens hiding (elements)

import Jvmhs
-- import Jvmhs.ClassPool

classpath :: [ FilePath ]
classpath =
  [ "test/data/classes" ]

getClassFromTestPool :: ClassName -> IO (Maybe Class)
getClassFromTestPool cn = do
  fmap snd . runTestClassPool $ getClass cn

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

beforeMethod ::
  AbsMethodName
  -> SpecWith Method
  -> Spec
beforeMethod mn =
  beforeClassPool $ do
    a <- getClass (mn^.inClassName)
    return $ a ^?! _Just.classMethod (mn^.relMethodName)._Just

useOutputFolder ::
  String
  -> SpecWith a
  -> SpecWith a
useOutputFolder folder = beforeAll_ $ do
  _ <- tryIOError $ removeDirectoryRecursive folder
  createDirectoryIfMissing True folder

beforeJREClassPool ::
  [String]
  -> CachedClassPoolT ClassLoader IO a
  -> SpecWith a
  -> Spec
beforeJREClassPool cp scpt =
  beforeAll $ do
    r <- fromClassPath cp
    fst <$> runCachedClassPoolT scpt (defaultFromReader r)

runJREClassPool ::
  [String]
  -> CachedClassPoolT ClassLoader IO a
  -> IO a
runJREClassPool cp scpt =
  fmap fst . runCachedClassPoolT scpt . defaultFromReader =<< fromClassPath cp

withJREMethodIt ::
  (HasCallStack, Arg a ~ Method, Example a)
  => [String]
  -> AbsMethodName
  -> String
  -> (AbsMethodName -> a)
  -> Spec
withJREMethodIt cp mn n fn =
  beforeJREClassPool cp (fmap fromJust $ getMethod mn) $ do
    it (printf "%s (%s)" n (show mn)) $ fn mn

withJREClassMethods ::
  (HasCallStack, Example b)
  => [String]
  -> ClassName
  -> String
  -> (AbsMethodName -> Method -> b)
  -> SpecWith (Arg b)
withJREClassMethods cp cn n fn = do
  mcls <- runIO . runJREClassPool cp $ getClass cn
  forM_ mcls $ \cls -> do
    forMOf_ (classMethods.folded) cls $ \m -> do
      let mn = mkAbsMethodName (cls^.className) (m^.methodName)
      it (printf "%s (%s)" n (show mn)) $ fn mn m

getJREHierachy :: [String] -> FilePath -> IO (Maybe Hierarchy)
getJREHierachy cp fp = do
  doesFileExist fp >>= \case
    True -> do
      Just stubs <- fmap decode $ BL.readFile fp
      return . Just . snd $ calculateHierarchy stubs
    False -> fmap (either (\(SomeException _) -> Nothing) Just). try $ do
      r <- preload =<< fromClassPath cp
      (_, st) <- loadClassPoolState (defaultFromReader r)
      hry <- fmap fst . flip runClassPoolT st . fmap snd $ getHierarchy
      BL.writeFile fp $ encode (hry^.hryStubs)
      return hry
