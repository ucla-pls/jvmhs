{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.JVM.Hierarchy
  ( Hierarchy
  , getClass
  , getState

  , clazz
  , classes

  , runHierarchy
  , runHierarchyClassPath
  , runHierarchyClassLoader
  , runHierarchyClassPreloader

  -- , allInterfaces
  -- , indirectInterfaces
  -- , directInterfaces

  -- , implementedMethods
  -- , interfaceMethods
  ) where

import           Language.JVM.Class
import           Language.JVM.ClassLoader
import           Language.JVM.ClassName

import           Control.Monad.Free

import           Control.Lens
import           Control.Lens.Action

import qualified Data.Map as M

data HierarchyLang a
  = GetClass ClassName (Class -> a)
  | GetState (HierarchyState -> a)
  deriving (Functor)

type Hierarchy = Free HierarchyLang

getClass :: ClassName -> Hierarchy Class
getClass classname =
  liftF $ GetClass classname id

getState :: Hierarchy HierarchyState
getState =
  liftF $ GetState id

data HierarchyState = HierarchyState
  { _classes :: M.Map ClassName Class }

makeLenses ''HierarchyState

runHierarchy
  :: (ClassName -> IO [(FilePath,Either String Class)])
  -> Hierarchy a
  -> IO a
runHierarchy clf h =
  go M.empty h
  where
    go m (Free h') =
      case h' of
        GetClass cn f -> do
          case M.lookup cn m of
            Just clz -> go m (f clz)
            Nothing -> do
              clzes <- clf cn
              case clzes of
                (_, Right clz) : _ -> go (M.insert cn clz m) (f clz)
                (_, Left msg) : _ -> fail msg
                [] -> fail $ "Couldn't find " ++ show cn
        GetState f ->
          go m (f $ HierarchyState m)
    go _ (Pure a) =
      return a

clazz :: Action Hierarchy ClassName Class
clazz = act getClass

-- directInterfaces :: Action Hierarchy ClassName [ClassName]
-- directInterfaces =
--   cls.interfaces

-- indirectInterfaces :: Action Hierarchy ClassName [ClassName]
-- indirectInterfaces =
--   cls.interfaces

-- allInterfaces :: Action Hierarchy ClassName [ClassName]
-- allInterfaces =
--   directInterfaces . traverse . directInterfaces

-- indirectInterfaces :: ClassName -> Hierarchy [ClassName]
-- indirectInterfaces cn = do
--   cls.interfaces.traverse
--   inter <- directInterfaces cn
--   S.unions <$> mapM allInterfaces (S.toList inter)

-- allInterfaces :: ClassName -> Hierarchy (S.Set ClassName)
-- allInterfaces cn = do
--   inter <- directInterfaces cn
--   S.unions . (inter:) <$> mapM allInterfaces (S.toList inter)

-- interfaceMethods :: ClassName -> Hierarchy (M.Map Method.MethodId ClassName)
-- interfaceMethods cn = do
--   inter <- S.toList <$> allInterfaces cn
--   M.unions <$> mapM (\cn -> hview (toMap cn) cn) inter

--   where
--     toMap :: ClassName -> Getter Class (M.Map Method.MethodId ClassName)
--     toMap cn =
--       methods
--       . to (map (view $ Method.identifier . to (,cn)))
--       . to M.fromList

-- implementedMethods :: ClassName -> Hierarchy [ (ClassName, Method.Method) ]
-- implementedMethods cn = do
--   methodMap <- interfaceMethods cn
--   clz_methods <- hview methods cn

--   return . flip map clz_methods $ \m ->
--     (maybe cn id $ M.lookup (m ^. Method.identifier) methodMap, m)

-- Helpers

-- | 'runHierarchyClassPreloader' runs the Hierarchy monad in the IO monad
-- using a 'ClassPreloader'. Using a ClassPreloader will be faster if working
-- with multiple files.
runHierarchyClassPreloader
  :: ClassPreloader
  -> Hierarchy a
  -> IO a
runHierarchyClassPreloader cl h = do
  runHierarchy (loadClassFromPreloader cl) h

runHierarchyClassLoader :: ClassLoader -> Hierarchy a -> IO a
runHierarchyClassLoader cl h = do
  runHierarchy (loadClass cl) h

runHierarchyClassPath :: [FilePath] -> Hierarchy a -> IO a
runHierarchyClassPath classpath h = do
  cl <- fromClassPath classpath
  runHierarchyClassLoader cl h
