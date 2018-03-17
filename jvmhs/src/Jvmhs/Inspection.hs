{-|
Module      : Jvmhs.Inspection
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module inspects the bytecode data structure.

-}

module Jvmhs.Inspection
  where

import Control.Lens
import qualified Data.Set as S

import Jvmhs.Data.Class
import Jvmhs.Data.Type
import Jvmhs.Hierarchy

-- import qualified Language.JVM as B
-- import qualified Language.JVM.Attribute.BootstrapMethods as B
-- import qualified Language.JVM.Attribute.Code as B

class Inspectable a where
  classNames :: Traversal' a ClassName
  classNames _ = pure

-- | Computes the class closure in the current space.
-- It will only include classes on known to the 'MonadHierarchy'.
computeClassClosure ::
  MonadHierarchy m
  => S.Set ClassName
  -> m (S.Set ClassName)
computeClassClosure =
  go S.empty
  where
    -- go :: S.Set ClassName -> S.Set ClassName -> m (S.Set ClassName)
    go known wave
      | S.null wave =
        return $ known
      | otherwise = do
        -- List of all the classes that exists
        exists <- wave ^!! folded.load'._Right
        let
          existsNames = S.fromList $ exists ^.. traverse . className
          known' = known `S.union` existsNames
          front = S.fromList $ exists^..folded.classNames
          -- Create a set of all the classnames we can load from
        go known' (front S.\\ known')


instance Inspectable Class where
  classNames g c =
    Class
      <$> g (_className c)
      <*> g (_classSuper c)
      <*> pure (_classAccessFlags c)
      <*> traverse g (_classInterfaces c)
      <*> traverse (classNames g) (_classFields c)
      <*> traverse (classNames g) (_classMethods c)
      <*> traverse (classNames g) (_classBootstrapMethods c)

instance Inspectable Field where
  classNames g f =
    Field
      <$> pure (_fieldAccessFlags f)
      <*> pure (_fieldName f)
      <*> classNames g (_fieldDescriptor f)
      <*> traverse (classNames g) (_fieldConstantValue f)

instance Inspectable Method where
  classNames g m =
    Method
      <$> pure (_methodAccessFlags m)
      <*> pure (_methodName m)
      <*> classNames g (_methodDescriptor m)
      <*> traverse (classNames g) (_methodCode m)
      <*> traverse g (_methodExceptions m)

instance Inspectable BootstrapMethod where

instance Inspectable Code where

instance Inspectable FieldDescriptor where
  classNames g fd =
    FieldDescriptor
      <$> classNames g (fieldDescriptorType fd)

instance Inspectable MethodDescriptor where
  classNames g md =
    MethodDescriptor
      <$> traverse (classNames g) (methodDescriptorArguments md)
      <*> traverse (classNames g) (methodDescriptorReturnType md)

instance Inspectable JValue where

instance Inspectable JType where
  classNames g t =
    case t of
      JTClass cn -> JTClass <$> g cn
      JTArray t' -> JTArray <$> classNames g t'
      a -> pure a
