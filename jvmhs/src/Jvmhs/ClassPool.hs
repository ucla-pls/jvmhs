{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-
Module      : Jvmhs.ClassPool
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the 'MonadClassPool' type class, and multiple
implementations.

-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Jvmhs.ClassPool
  (
  -- * MonadClassPool
    MonadClassPool (..)
  -- ** Basic operations
  , getClass
  , setClass
  , putClass
  , deleteClass

  , hasClass
  , allClasses
  , getClasses

  , onlyClasses
  , onlyClasses'

  , modifyClass
  , modifyClasses


  -- ** Lens helpers
  , pool
  , pool'
  , (^!!)
  , (^!)

  -- ** Loading classes into the `ClassPool`
  , loadClass
  , loadClasses
  , loadClassesFromReader
  , ClassPoolReadError

  -- ** Saving the class to disk
  , saveClass
  , saveAllClasses

  -- * Implementations

  -- ** ClassPoolT
  -- A pure implementation of the `MonadClassPool` which saves all the classes
  -- in a map. The structure is called `ClassPoolState`.
  , ClassPoolState
  , loadClassPoolState
  , saveClassPoolState

  , ClassPoolT (..)
  , ClassPool
  , runClassPool
  , runClassPoolT
  , runClassPoolTWithReader


  -- ** CachedClassPoolT
  -- An implementation that loads a cache of all classes instead of having
  -- to load all values up front.

  , CachedClassPoolT (..)
  , CachedClassPool

  , runCachedClassPool
  , runCachedClassPoolT

  ) where

-- import           Control.Monad          (foldM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT, mapStateT, runStateT)

import           Control.Lens
import           Control.Lens.Action

import           Data.Either                (partitionEithers)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Maybe
import           Data.Monoid
import           Data.Foldable as F

import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS

import           Debug.Trace

import           Jvmhs.ClassReader
import           Jvmhs.Data.Class
import           Jvmhs.Data.Type

-- | A Monad ClassPool is a modifiable pool of classes. It defines actions
-- over the class pool. The monad class pool it keeps track of

  -- a set of classes and it has the ability to change the class pool.
class Monad m => MonadClassPool m where
  -- | The `alterClass` function is the most general access into the class pool
  alterClass ::
    (Functor f) =>
    (Maybe Class -> f (Maybe Class))
    -> ClassName
    -> m (f (m ()))

  -- | Perform an action over all the classes in the ClassPool
  traverseClasses ::
    (Applicative f) =>
    (Class -> f (Maybe Class))
    -> m (f (m ()))

  -- | Returns a list of class names, each class name is guaranteed to only
  -- appear once in the
  allClassNames ::
    m [ClassName]

  -- | Saves only the classes provided in the foldable list of classnames.
  -- Names not found in the ClassPool are ignored.
  -- @
  -- saveClasses "out" ["java.util.ArrayList", "java.util.LinkedList"]
  -- @
  saveClasses ::
      (MonadIO m)
    => FilePath
    -> [ClassName]
    -> m ()
  saveClasses fp cns = do
    void . liftIO . writeClasses fp =<< (cns ^!! folded . pool . _Just)

  -- | Run a command in a local environment
  cplocal ::
    m a
    -> m a

  restrictTo ::
       S.Set ClassName
    -> m ()

-- | Get a class from a class pool.
getClass :: MonadClassPool m => ClassName -> m (Maybe Class)
getClass cn =
  getConst <$> alterClass (Const) cn

-- | Set a class on the class pool.
setClass :: MonadClassPool m => ClassName -> Maybe Class -> m ()
setClass cn mc =
  join $ runIdentity <$> alterClass (\_ -> Identity mc) cn

-- | Modify a class on the class pool.
modifyClass :: MonadClassPool m => ClassName -> (Maybe Class -> Maybe Class) -> m ()
modifyClass cn fn =
  join $ runIdentity <$> alterClass (Identity . fn) cn

-- | Map classes, map over the classes in the class pool
modifyClasses :: MonadClassPool m => (Class -> Maybe Class) -> m ()
modifyClasses fn =
  join $ runIdentity <$> traverseClasses (Identity . fn)

-- | Check if class exist in class pool.
hasClass :: MonadClassPool m => ClassName -> m Bool
hasClass = fmap isJust . getClass

-- | Put a class in the class pool.
putClass :: MonadClassPool m => Class -> m ()
putClass cls =
  setClass (cls^.className) (Just cls)

-- | Deletes a class in the class pool.
deleteClass :: MonadClassPool m => ClassName -> m ()
deleteClass cn =
  setClass cn Nothing

-- | Deletes all classes not in the list
onlyClasses :: (MonadClassPool m, Foldable f) => f ClassName -> m ()
onlyClasses cns = do
  onlyClasses' $ S.fromList (F.toList cns)

-- | Deletes all classes not in the set
onlyClasses' :: (MonadClassPool m) => S.Set ClassName -> m ()
onlyClasses' =
  restrictTo

-- | Get all the classes from the class pool
allClasses :: MonadClassPool m => m [Class]
allClasses =
  flip appEndo [] . getConst <$> traverseClasses (Const . Endo . (:))

-- | A pool action can be used as part of a lens to access a class.
-- >>> "java.lang.Object" ^! pool.className
-- "java.lang.Object"
pool ::
     MonadClassPool m
  => Action m ClassName (Maybe Class)
pool = act getClass

-- | A specialized pool action that returns the name of the class if
-- it does not exist in the ClassPool
pool' ::
     MonadClassPool m
  => Action m ClassName (Either ClassName Class)
pool' = act (\cn -> maybe (Left cn) (Right) <$> getClass cn)

-- | Get all the classes from the class pool
getClasses :: (Foldable t, MonadClassPool m) => t ClassName -> m [Class]
getClasses =
  (^!! folded.pool._Just)

-- | Given a ClassReader load a class into the ClassPool
loadClass ::
  (MonadClassPool m, MonadClassReader r m)
  => ClassName
  -> m (Either ClassReadError Class)
loadClass cn = do
  e <- readClassM cn
  case e of
    Left err -> return $ Left err
    Right cls -> do
      putClass cls
      return $ Right cls

type ClassPoolReadError = (ClassName, ClassReadError)

-- | Load all the classes into a the class pool
loadClasses ::
  (MonadClassPool m, MonadClassReader r m)
  => m [ClassPoolReadError]
loadClasses = do
  (errs, clss) <- partitionEithers <$> readClassesM
  mapM_ putClass clss
  return errs

-- | Load all the classes from a class reader into the class reader
loadClassesFromReader ::
  (ClassReader r, MonadIO m, MonadClassPool m)
  => r -> m [ClassPoolReadError]
loadClassesFromReader =
  runClassReaderT loadClasses

-- | Saves all the classes in the pool to the filepath.
saveAllClasses ::
    (MonadClassPool m, MonadIO m)
  => FilePath
  -> m ()
saveAllClasses fp = do
  allClassNames >>= saveClasses fp

-- | Saves a single class to the given class path.
-- @
-- saveClass "out" "java.util.ArrayList"
-- @
saveClass ::
    (MonadClassPool m, MonadIO m)
  => FilePath
  -> ClassName
  -> m ()
saveClass fp cn = do
  saveClasses fp [cn]


instance MonadClassPool m => MonadClassPool (ReaderT r m) where
  alterClass f cn = lift $ fmap (fmap lift) $ alterClass f cn
  allClassNames = lift allClassNames
  traverseClasses f =
    lift $ fmap (fmap lift) $ traverseClasses f
  cplocal m = do
    r <- ask
    lift . cplocal $ runReaderT m r

  restrictTo = lift . restrictTo


-- | The class pool state is just a map from class to class names
type ClassPoolState = M.Map ClassName Class

-- | Load the class pool state upfront
loadClassPoolState ::
  (ClassReader r, MonadIO m)
  => r
  -> m ([ClassPoolReadError], ClassPoolState)
loadClassPoolState =
  runClassPoolTWithReader return

-- | Save class pool state to a file-path
saveClassPoolState ::
  FilePath
  -> ClassPoolState
  -> IO ()
saveClassPoolState fp =
  fmap fst . runClassPoolT (saveAllClasses fp)

-- | `ClassPoolT` is a simple wrapper around `StateT`
newtype ClassPoolT m a = ClassPoolT
  { runClassPoolT' :: StateT ClassPoolState m a }
  deriving (Functor, Applicative, Monad, MonadState ClassPoolState, MonadTrans, MonadIO)

instance MonadReader r m => MonadReader r (ClassPoolT m) where
  ask = lift ask
  reader = lift . reader
  local f = ClassPoolT . (mapStateT $ local f) . runClassPoolT'

instance Monad m => MonadClassPool (ClassPoolT m) where
  alterClass f n = do
    fmap put . M.alterF f n <$> get

  -- | Perform an action over all the classes in the ClassPool
  traverseClasses f = do
    fmap put . M.traverseMaybeWithKey (\_ cls -> f cls) <$> get

  allClassNames =
     M.keys <$> get

  cplocal m = do
    cp <- get
    lift $ fst <$> runClassPoolT m cp

  restrictTo s =
    modify (flip M.restrictKeys s)

type ClassPool = ClassPoolT Identity

-- | Run a `ClassPoolT` given a class pool
runClassPoolT ::
  ClassPoolT m a
  -> ClassPoolState
  -> m (a, ClassPoolState)
runClassPoolT = runStateT . runClassPoolT'

-- | Run a `ClassPoolT` given a class pool
runClassPool ::
  ClassPool a
  -> ClassPoolState
  -> (a, ClassPoolState)
runClassPool m = runIdentity . runClassPoolT m

-- | Run a `MonadClassPool` but load all values from a class loader
runClassPoolTWithReader ::
     (ClassReader r, MonadIO m)
  => ([ClassPoolReadError] -> ClassPoolT m a)
  -> r
  -> m (a, ClassPoolState)
runClassPoolTWithReader fm r =
  runClassPoolT (loadClassesFromReader r >>= fm) mempty

data ClassState
  = CSSaved Class BL.ByteString
  | CSPure Class
  | CSUnread

type CachedClassPoolState = M.Map ClassName ClassState

newtype CachedClassPoolT r m a = CachedClassPoolT
  { runCachedClassPoolT' :: StateT CachedClassPoolState (ReaderT r m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader r
    , MonadState CachedClassPoolState
    , MonadIO )

instance MonadTrans (CachedClassPoolT r) where
  lift m = CachedClassPoolT (lift $ lift m)

cacheClass ::
  (ClassReader r, MonadIO m)
  => ClassName
  -> CachedClassPoolT r m CachedClassPoolState
cacheClass n = do
  cpp <- M.alterF cache n =<< get
  put cpp
  return cpp
  where
    cache (Just CSUnread) = do
      traceM $ "Caching.. " ++ show n
      ec <- liftIO . readClass n =<< ask
      return $ case ec of
        Left _ -> Nothing
        Right cls -> Just (CSPure cls)
    cache mc =
      return mc

-- cachedOnlyClasses' ::
--   (ClassReader r, MonadIO m)
--   => S.Set ClassName
--   -> CachedClassPoolT r m ()
-- cachedOnlyClasses' keep = do
--   cns <- allClassNames
--   modify $
--     \s -> M.fromList . (fmap $
--     \cn -> (cn,
--             if cn `S.member` keep
--             then fromMaybe Nothing $ cn `M.lookup` s
--             else Nothing)) $ cns

inMaybeClassState ::
  (Functor f)
  => (Maybe Class -> f (Maybe Class))
  -> Maybe ClassState
  -> f (Maybe ClassState)
inMaybeClassState f x =
  case x of
    Nothing -> fmap CSPure <$> f Nothing
    Just x' -> inClassState (f . Just) x'

inClassState ::
  (Functor f)
  => (Class -> f (Maybe Class))
  -> ClassState
  -> f (Maybe ClassState)
inClassState f x =
  fmap CSPure <$> case x of
    CSPure cs -> f cs
    CSSaved cs _ -> f cs
    CSUnread -> error "Should not happen!"

instance (ClassReader r, MonadIO m) => MonadClassPool (CachedClassPoolT r m) where
  -- alterClass ::
  --   forall f. (Functor f) =>
  --   (Maybe Class -> f (Maybe Class))
  --   -> ClassName
  --   -> m (f (m ()))
  alterClass f n = do
    fmap put . M.alterF (inMaybeClassState f) n <$> cacheClass n

  -- | Perform an action over all the classes in the ClassPool
  traverseClasses f = do
    mapM_ cacheClass =<< allClassNames
    fmap put . M.traverseMaybeWithKey (\_ -> inClassState f) <$> get

  allClassNames =
    M.keys <$> get

  -- | Saves only the classes provided in the foldable list of classnames.
  -- Names not found in the ClassPool are ignored.
  -- @
  -- saveClasses "out" ["java.util.ArrayList", "java.util.LinkedList"]
  -- @
  -- saveClasses ::
  --     (MonadIO m, Foldable t)
  --   => FilePath
  --   -> t ClassName
  --   -> m ()
  saveClasses fp cns = do
    bs <- forM cns $ \cn -> do
      cs <- cacheClass cn
      let (bs, mp) = M.alterF helper cn cs
      put mp
      return ((cn,) <$> bs)
    liftIO . writeBytesToFilePath fp $ catMaybes bs
    where
      helper Nothing = (Nothing, Nothing)
      helper (Just (CSPure cs)) =
        let bs = deserializeClass cs in
        (Just bs, Just (CSSaved cs bs))
      helper this@(Just (CSSaved _ bs)) =
        (Just bs, this)
      helper (Just CSUnread) =
        error "Should have been read"

  cplocal (CachedClassPoolT m) = do
    cp <- get
    r <- ask
    lift $ fst <$>
      (flip runReaderT r . flip runStateT cp $ m)

  restrictTo s =
    modify (flip M.restrictKeys s)

runCachedClassPoolT ::
     (ClassReader r, MonadIO m)
  => CachedClassPoolT r m a
  -> r
  -> m (a, CachedClassPoolState)
runCachedClassPoolT (CachedClassPoolT m) r = do
  cns <- liftIO $ classes r
  flip runReaderT r . flip runStateT (M.fromList [ (cn,CSUnread) | (cn,_) <- cns ]) $ m

type CachedClassPool = CachedClassPoolT ClassPreloader IO

-- | Run a `CachedClassPool` given a class pool
runCachedClassPool ::
  CachedClassPool a
  -> ClassPreloader
  -> IO (a, CachedClassPoolState)
runCachedClassPool =
  runCachedClassPoolT
