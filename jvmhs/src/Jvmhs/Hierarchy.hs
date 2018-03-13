{-
Module      : Jvmhs.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines a class Hierarchy. The class hierarchy, contains
every class loaded by the program.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Jvmhs.Hierarchy
  (
    HierarchyError (..)
  , Hierarchy
  , HierarchyState (..)
  , runHierarchy
  , runHierarchyInClassPath

  , loadClass
  , load
  , deepLoadClass
  ) where

import           Control.Monad          (foldM)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, runStateT)

import           Control.Lens
import           Control.Lens.Action

import           Data.Map               as Map
import           Data.Set               as Set

import           Jvmhs.ClassReader
import           Jvmhs.Data.Class

data HierarchyState r = HierarchyState
  { _loadedClasses :: Map.Map ClassName Class
  , _classReader   :: r
  }
  deriving (Show, Eq)

makeLenses ''HierarchyState

data HierarchyError
  = ErrorWhileReadingClass ClassName ClassReadError
  deriving (Show, Eq)

type Hierarchy r = StateT (HierarchyState r) (ExceptT HierarchyError IO)

runHierarchy
  :: ClassReader r
  => Hierarchy r a
  -> HierarchyState r
  -> IO (Either HierarchyError (a, HierarchyState r))
runHierarchy h  =
  runExceptT . runStateT h

runHierarchyInClassPath
  :: [ FilePath ]
  -> Hierarchy ClassPreloader a
  -> IO (Either HierarchyError (a, HierarchyState ClassPreloader))
runHierarchyInClassPath cp hc = do
  ld <- fromClassPath cp
  p <- preload ld
  runHierarchy hc (HierarchyState (Map.empty) p)

-- | Load a class into the hierarchy if not already loaded.
loadClass
  :: ClassReader r
  => ClassName
  -> Hierarchy r Class
loadClass cn = do
  x <- use $ loadedClasses . at cn
  case x of
    Just l ->
      return l
    Nothing -> do
      r <- use classReader
      l <- liftIO $ readClass r cn
      case l of
        Left err ->
          throwError $ ErrorWhileReadingClass cn err
        Right cls -> do
          loadedClasses . at cn .= Just cls
          return cls

-- | An load action can be used as part of a lens to load a
-- class.
load
  :: ClassReader r
 => Action (Hierarchy r) ClassName Class
load = act loadClass

-- | Load a class and all it's dependencies into the hierarchy.
deepLoadClass
  :: ClassReader r
  => ClassName
  -> Hierarchy r (Class, Set.Set ClassName)
deepLoadClass =
  go Set.empty
  where
    go loaded cn
      | cn `Set.member` loaded = do
        cls <- loadClass cn
        return (cls, loaded)
      | otherwise = do
        cls <- loadClass cn
        l' <- foldM
              (\l cn' -> snd <$> go l cn')
              (Set.insert cn loaded)
              (dependencies cls)
        return (cls, l')
