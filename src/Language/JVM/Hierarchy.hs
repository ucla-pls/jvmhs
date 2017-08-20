{-# LANGUAGE DeriveFunctor #-}
module Language.JVM.Hierarchy
  (
  ) where

import           Language.JVM.Class
import           Language.JVM.ClassLoader
import           Language.JVM.ClassName

import           Control.Monad.Free

import qualified Data.Map as M

data HierarchyF a
  = GetClass ClassName (Class -> a)
  | Fail String
  deriving (Functor)

type Hierarchy = Free HierarchyF

getClass :: ClassName -> Hierarchy Class
getClass classname =
  Free $ GetClass classname Pure

runHierarchy :: ClassLoader -> Hierarchy a -> IO a
runHierarchy cl h =
  go M.empty h
  where
    go m (Free h) =
      case h of
        GetClass cn f -> do
          case M.lookup cn m of
            Just clz -> go m (f clz)
            Nothing -> do
              eclz <- snd . head <$> loadClass cn cl
              case eclz of
                Right clz -> go (M.insert cn clz m) (f clz)
                Left msg -> fail msg
    go m (Pure a) =
      return a

runHierarchyClassPath :: [FilePath] -> Hierarchy a -> IO a
runHierarchyClassPath classpath h = do
  cl <- simple classpath
  runHierarchy cl h

superClass :: Class -> Hierarchy Class
superClass clz =
  getClass (super clz)
