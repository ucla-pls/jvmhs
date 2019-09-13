{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-
Module      : Jvmhs.ClassPool
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module defines the 'MonadClassPool' type class, and multiple
implementations.

-}
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

  , mapClasses
  , collectClasses
  , modifyClassesM
  , destroyClassesM

  -- ** Access Methods and Fields
  , getMethod


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

  , streamClasses
  -- , streamClasses'

  ) where

-- import           Control.Monad          (foldM)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT (..), mapStateT)

-- transformers
import           Data.Functor.Compose
import           Data.Hashable

-- mtl
import           Control.Monad.Identity
import           Control.Monad.Writer

-- lens
import           Control.Lens

-- lens-action
import           Control.Lens.Action

-- base
import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S
import           Data.Maybe
import           Data.Foldable              as F

-- bytestring
import qualified Data.ByteString.Lazy       as BL

-- jvmhs
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

  -- | Alter all classes
  alterClasses ::
    (MonadTrans t, Monad (t m))
    => (Class -> t m (Maybe Class))
    -> t m ()

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
  cplocal :: m a -> m a

  restrictTo :: S.HashSet ClassName -> m ()


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
  runIdentityT $ alterClasses (IdentityT . return . fn)

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
onlyClasses' :: (MonadClassPool m) => S.HashSet ClassName -> m ()
onlyClasses' =
  restrictTo

-- | Get all the classes from the class pool
allClasses :: MonadClassPool m => m [Class]
allClasses =
  mapClasses id

-- | Get all the classes from the class pool
collectClasses :: forall r m. ( Monoid r, MonadClassPool m ) => (Class -> r) -> m r
collectClasses f =
  execWriterT . alterClasses
  $ \c -> tell (f c) >> return (Just c)

-- | Get all the classes from the class pool
mapClasses :: MonadClassPool m => (Class -> r) -> m [r]
mapClasses f =
  flip appEndo [] <$> collectClasses (\c -> Endo (f c:))

-- | Modify all the classes but keep side-effects
modifyClassesM ::
  MonadClassPool m
  => (Class -> m (Maybe Class))
  -> m ()
modifyClassesM fn =
  runIdentityT $ alterClasses (lift . fn)

-- | Destroy all classes in the class pool, but not before loading all of them
destroyClassesM ::
  MonadClassPool m
  => (Class -> m ())
  -> m ()
destroyClassesM fn =
  modifyClassesM (fn >=> const (return Nothing))

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
  r <- ask
  classnames <- liftIO $ classes (classReader r)
  errs <- forM classnames $ \(cn, con) ->
    liftIO (readClass cn (const con <$> r)) >>= \case
       Right cls -> do
         putClass cls
         return Nothing
       Left err ->
         return $ Just (cn, err)
  return $ catMaybes errs

-- | Load all the classes from a class reader into the class reader
loadClassesFromReader ::
  (ClassReader r, MonadIO m, MonadClassPool m)
  => ReaderOptions r -> m [ClassPoolReadError]
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


getMethod ::
  (MonadClassPool m )
  => AbsMethodName
  -> m (Maybe Method)
getMethod mid = do
  cls <- getClass (mid^._1)
  return $ cls^?_Just.classMethod(mid^._2)._Just

instance MonadClassPool m => MonadClassPool (ReaderT r m) where
  alterClass f cn = lift $ fmap (fmap lift) $ alterClass f cn
  alterClasses f = alterClasses f
  allClassNames = lift allClassNames
  -- traverseClasses f =
  --   lift $ fmap (fmap lift) $ traverseClasses f
  cplocal m = do
    r <- ask
    lift . cplocal $ runReaderT m r

  restrictTo = lift . restrictTo


-- | The class pool state is just a map from class to class names
type ClassPoolState = M.HashMap ClassName Class

-- | Load the class pool state upfront
loadClassPoolState ::
  (ClassReader r, MonadIO m)
  => ReaderOptions r
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
    fmap put . alterF f n <$> get

  alterClasses f =
    lift get >>= fmap (M.mapMaybe id) . M.traverseWithKey (\_ cls -> f cls) >>= lift . put
  -- -- | Perform an action over all the classes in the ClassPool
  -- traverseClasses f = do
  --   fmap put . M.traverseMaybeWithKey (\_ cls -> f cls) <$> get

  allClassNames =
     M.keys <$> get

  cplocal m = do
    cp <- get
    lift $ fst <$> runClassPoolT m cp

  restrictTo s =
    modify (flip M.intersection (S.toMap s))

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
  -> ReaderOptions r
  -> m (a, ClassPoolState)
runClassPoolTWithReader fm r =
  runClassPoolT (loadClassesFromReader r >>= fm) mempty

data ClassState
  = CSSaved Class BL.ByteString
  | CSPure Class
  | CSUnread

type CachedClassPoolState = M.HashMap ClassName ClassState

newtype CachedClassPoolT r m a = CachedClassPoolT
  { runCachedClassPoolT' :: StateT CachedClassPoolState (ReaderT (ReaderOptions r) m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState CachedClassPoolState
    , MonadIO )

instance MonadTrans (CachedClassPoolT r) where
  lift = CachedClassPoolT . lift . lift

instance (MonadReader r m) => MonadReader r (CachedClassPoolT r' m) where
  reader f = lift (reader f)
  local f m =
    CachedClassPoolT . StateT $ \s ->
      ReaderT (local f . runReaderT (runStateT (runCachedClassPoolT' m) s))


traverseState ::
  (Functor f)
  => (Maybe Class -> f (Maybe Class))
  -> ClassState -> f (Maybe ClassState)
traverseState fn x =
  fmap CSPure <$> case x of
    CSPure cs    -> fn (Just cs)
    CSSaved cs _ -> fn (Just cs)
    CSUnread     -> fn (Nothing)


tryEnsureClass ::
  (ClassReader r, MonadIO m)
  => ClassName
  -> ReaderOptions r
  -> Maybe Class -> m (Maybe Class)
tryEnsureClass n opt = \case
  Nothing -> liftIO (preview _Right <$> readClass n opt)
  es -> return es

-- | Iterate through all classes, but does not cache anything.
streamClasses ::
  (ClassReader r, MonadIO m)
  => (Class -> m ())
  -> CachedClassPoolT r m ()
streamClasses fn = do
  cplocal $ destroyClassesM (lift . fn)

instance (ClassReader r, MonadIO m) => MonadClassPool (CachedClassPoolT r m) where

  alterClass f n = do
    opt <- CachedClassPoolT ask
    m <- liftIO . getCompose . alterF (alterer opt) n =<< get
    return (fmap put m)
    where
      alterer opt =
        helper (Compose . fmap f . tryEnsureClass n opt )

      helper fn =
        maybe (fmap CSPure <$> fn Nothing) (traverseState fn)

  alterClasses f = do
    opt <- lift $ CachedClassPoolT ask
    lift get
      >>= fmap (M.mapMaybe id) . M.traverseWithKey
      (\n cls ->
        traverseState (lift . tryEnsureClass n opt >=> maybe (return Nothing) f) cls
      )
      >>= lift . put

  allClassNames =
    M.keys <$> get

  saveClasses fp cns = do
    bs <- forM cns $ \cn -> do
      modifyClass cn id
      cs <- get
      let (bs, mp) = alterF helper cn cs
      put mp
      return ((cn,) <$> bs)
    liftIO . writeBytesToFilePath fp $ catMaybes bs
    where
      helper Nothing = (Nothing, Nothing)
      helper (Just (CSPure cs)) =
        let bs = serializeClass cs in
        (Just bs, Just (CSSaved cs bs))
      helper this@(Just (CSSaved _ bs)) =
        (Just bs, this)
      helper (Just CSUnread) =
        error "Should have been read"

  cplocal (CachedClassPoolT m) = do
    cp <- get
    r <- CachedClassPoolT ask
    lift $ fst <$>
      (flip runReaderT r . flip runStateT cp $ m)

  restrictTo s =
    modify (flip M.intersection (S.toMap s))

runCachedClassPoolT ::
     (ClassReader r, MonadIO m)
  => CachedClassPoolT r m a
  -> ReaderOptions r
  -> m (a, CachedClassPoolState)
runCachedClassPoolT (CachedClassPoolT m) r = do
  cns <- liftIO . classes $ classReader r
  runReaderT ( runStateT m (M.fromList [ (fst c,CSUnread) | c <- cns ])) r

type CachedClassPool =
  CachedClassPoolT ClassPreloader IO

-- | Run a `CachedClassPool` given a class pool
runCachedClassPool ::
  CachedClassPool a
  -> ReaderOptions ClassPreloader
  -> IO (a, CachedClassPoolState)
runCachedClassPool =
  runCachedClassPoolT

alterF ::
  (Eq k, Hashable k, Functor f)
  => (Maybe v -> f (Maybe v))
  -> k -> M.HashMap k v
  -> f (M.HashMap k v)
alterF f k m =
  flip fmap (f (M.lookup k m)) $ \case
    Nothing -> M.delete k m
    Just v  -> M.insert k v m
