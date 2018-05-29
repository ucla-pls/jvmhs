{-
Module      : Jvmhs.ClassPool
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines a class ClassPool. The class 'MonadClassPool', contains
every class loaded by the program.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Jvmhs.ClassPool
  ( MonadClassPool (..)
  , loadClass'

  , ClassPoolError (..)
  , heClassName
  , heClassReadError

   -- * ClassPool implementation
  , ClassPool
  , runClassPool
  , runClassPool'
  , runClassPoolInClassPath
  , runClassPoolInClassPathOnly

  , dumpClassPool

  , ClassPoolState (..)
  , saveClassPoolState
  , savePartialClassPoolState
  , emptyState

  -- * Helpers
  , load
  , load'
  , (^!!)
  , (^!)
  , module Control.Monad.Except
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
import           Jvmhs.Data.Type

data ClassPoolState r = ClassPoolState
  { _loadedClasses :: Map.Map ClassName Class
  , _classReader   :: r
  }
  deriving (Show, Eq)

makeLenses ''ClassPoolState

data ClassPoolError
  = ErrorWhileReadingClass
  { _heClassName :: ClassName
  , _heClassReadError :: ClassReadError
  } deriving (Show, Eq)

makeLenses ''ClassPoolError

newtype ClassPool r a =
  ClassPool (StateT (ClassPoolState r) (ExceptT ClassPoolError IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ClassPoolError
    , MonadState (ClassPoolState r)
    , MonadIO
    )

class (MonadError ClassPoolError m, Monad m) => MonadClassPool m where
  loadClass :: ClassName -> m Class
  saveClass :: Class -> m ()
  -- | Behavior for changing classname in class is undefined.
  modifyClass :: ClassName -> (Class -> Class) -> m ()

saveClassPoolState :: FilePath -> ClassPoolState r -> IO ()
saveClassPoolState fp s =
  writeClasses fp (s^.loadedClasses)

dumpClassPool ::
  (MonadClassPool m, MonadIO m, Foldable t)
  => FilePath
  -> t ClassName
  -> m ()
dumpClassPool fp clss = do
  clss' <- clss ^!! folded.load
  liftIO $ writeClasses fp clss'

savePartialClassPoolState ::
     Foldable f
  => FilePath
  -> f ClassName
  -> ClassPoolState r
  -> IO ()
savePartialClassPoolState fp fs s =
  writeClasses fp (fs^..folded.to (flip Map.lookup cl)._Just)
  where
    cl = s^.loadedClasses

runClassPool
  :: ClassReader r
  => r
  -> ClassPool r a
  -> IO (Either ClassPoolError a)
runClassPool r h =
  fmap fst <$> runClassPool' h (emptyState r)

emptyState :: r -> ClassPoolState r
emptyState r = (ClassPoolState Map.empty r)

runClassPool'
  :: ClassReader r
  => ClassPool r a
  -> ClassPoolState r
  -> IO (Either ClassPoolError (a, ClassPoolState r))
runClassPool' (ClassPool h) =
  runExceptT . runStateT h

runClassPoolInClassPathOnly
  :: [ FilePath ]
  -> ClassPool ClassPreloader a
  -> IO (Either ClassPoolError a)
runClassPoolInClassPathOnly cp hc = do
  p <- preload $ fromClassPathOnly cp
  fmap fst <$> runClassPool' hc (emptyState p)

runClassPoolInClassPath
  :: [ FilePath ]
  -> ClassPool ClassPreloader a
  -> IO (Either ClassPoolError a)
runClassPoolInClassPath cp hc = do
  ld <- fromClassPath cp
  p <- preload ld
  fmap fst <$> runClassPool' hc (emptyState p)

instance ClassReader r => MonadClassPool (ClassPool r) where
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

  saveClass cls = do
    loadedClasses . at (cls^.className) .= Just cls

  modifyClass cn f = do
    cls <- loadClass cn
    saveClass (f cls)

-- | LoadClass'
loadClass' ::
  MonadClassPool m
  =>
  ClassName
  -> m (Maybe Class)
loadClass' cn =
  catchError
    (Just <$> loadClass cn)
    (pure . const Nothing)


-- | An load action can be used as part of a lens to load a
-- class.
load
  :: MonadClassPool m
  => Action m ClassName Class
load = act loadClass

-- | Loads a 'Class' but does not fail if the class was not loaded
-- instead it returns a ClassPoolError.
load' ::
     MonadClassPool m
  => Action m ClassName (Either ClassPoolError Class)
load' = act (\cn -> catchError (Right <$> loadClass cn) (return . Left))
