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
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Jvmhs.Hierarchy
  ( MonadHierarchy (..)
  , HierarchyError (..)

   -- * Hierarchy implementation
  , Hierarchy
  , HierarchyState (..)
  , runHierarchy
  , runHierarchy'
  , runHierarchyInClassPath

  -- * Helpers
  , load
  , load'
  , (^!!)
  , (^!)
  ) where

-- import           Control.Monad          (foldM)
import           Control.Monad.Except
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Class
import           Control.Monad.State    (StateT, runStateT)

import           Control.Lens
import           Control.Lens.Action

import           Data.Map               as Map
-- import           Data.Set               as Set

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

newtype Hierarchy r a =
  Hierarchy (StateT (HierarchyState r) (ExceptT HierarchyError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError HierarchyError
    , MonadState (HierarchyState r)
    , MonadIO
    )

class (MonadError HierarchyError m, Monad m) => MonadHierarchy m where
  loadClass :: ClassName -> m Class


runHierarchy
  :: ClassReader r
  => r
  -> Hierarchy r a
  -> IO (Either HierarchyError (a, HierarchyState r))
runHierarchy r h =
  runHierarchy' h (HierarchyState Map.empty r)

runHierarchy'
  :: ClassReader r
  => Hierarchy r a
  -> HierarchyState r
  -> IO (Either HierarchyError (a, HierarchyState r))
runHierarchy' (Hierarchy h) =
  runExceptT . runStateT h

runHierarchyInClassPath
  :: [ FilePath ]
  -> Hierarchy ClassPreloader a
  -> IO (Either HierarchyError (a, HierarchyState ClassPreloader))
runHierarchyInClassPath cp hc = do
  ld <- fromClassPath cp
  p <- preload ld
  runHierarchy' hc (HierarchyState (Map.empty) p)

instance ClassReader r => MonadHierarchy (Hierarchy r) where
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
  :: MonadHierarchy m
  => Action m ClassName Class
load = act loadClass

-- | Loads a 'Class' but does not fail if the class was not loaded
-- instead it returns a HierarchyError.
load'
  :: MonadHierarchy m
  => Action m ClassName (Either HierarchyError Class)
load' = act (\cn -> catchError (Right <$> loadClass cn) (return . Left))


-- -- | Load a class and all it's dependencies into the hierarchy.
-- deepLoadClass
--   :: ClassReader r
--   => ClassName
--   -> Hierarchy r (Class, Set.Set ClassName)
-- deepLoadClass =
--   go Set.empty
--   where
--     go loaded cn
--       | cn `Set.member` loaded = do
--         cls <- loadClass cn
--         return (cls, loaded)
--       | otherwise = do
--         cls <- loadClass cn
--         l' <- foldM
--               (\l cn' -> snd <$> go l cn')
--               (Set.insert cn loaded)
--               (dependencies cls)
--         return (cls, l')
