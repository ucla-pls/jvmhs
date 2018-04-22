{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Jvmhs.Data.Code
import Jvmhs.Data.Type
import Jvmhs.Hierarchy

-- import qualified Language.JVM as B
-- import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.Code as B
import qualified Language.JVM as B

class Inspectable a where
  classNames :: Traversal' a ClassName
  classNames _ = pure

nothing :: Traversal' a b
nothing = const pure
{-# INLINE nothing #-}

instance Inspectable Class where
  classNames =
    traverseClass
      id id nothing
      traverse
      (traverse.classNames)
      (traverse.classNames)
      (traverse.classNames)

instance Inspectable Field where
  classNames =
    traverseField
      nothing
      nothing
      classNames
      (traverse.classNames)

instance Inspectable Method where
  classNames =
    traverseMethod
      nothing
      nothing
      classNames
      (traverse.classNames)
      traverse

instance Inspectable BootstrapMethod where

instance Inspectable Code where
  classNames =
    traverseCode
      nothing
      nothing
      (traverse.classNames)
      (traverse.classNames)
      nothing

instance Inspectable ByteCodeOpr where
  classNames g o =
    case o of
      B.Push c -> B.Push <$> classNames g c
      B.Get fa c -> B.Get fa . B.deepRef <$> classNames g (B.deepValue c)
      B.Put fa c -> B.Put fa . B.deepRef <$> classNames g (B.deepValue c)
      B.Invoke r -> B.Invoke <$> classNames g r
      B.New c -> B.New . B.RefV <$> g (B.value c)
      B.NewArray c -> B.NewArray <$> classNames g c
      B.MultiNewArray c v -> flip B.MultiNewArray v . B.RefV <$> g (B.value c)
      B.CheckCast c -> B.CheckCast . B.RefV <$> g (B.value c)
      B.InstanceOf c -> B.InstanceOf . B.RefV <$> g (B.value c)
      _ -> pure o

instance Inspectable (B.CConstant B.High) where

instance Inspectable (B.Invokation B.High) where

instance Inspectable (B.AbsFieldId B.High) where

instance Inspectable (B.ExactArrayType B.High) where

instance Inspectable ExceptionTable where
  classNames =
    exceptionCatchType._Just

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

-- | Computes the class closure in the current space.
-- It will only include classes known to the 'MonadHierarchy'.
computeClassClosure ::
  MonadHierarchy m
  => S.Set ClassName
  -> m (S.Set ClassName)
computeClassClosure =
  go S.empty
  where
    -- go :: S.Set ClassName -> S.Set ClassName -> m (S.Set ClassName)
    go known wave
      | S.null wave = do
        return known
      | otherwise = do
        -- List of all the classes that exists
        exists <- wave ^!! folded . load' . _Right
        let
          existsNames = S.fromList $ exists ^.. folded . className
          known' = known `S.union` existsNames
          front = S.fromList $ exists^..folded.classNames
          -- Create a set of all the classnames we can load from
        go known' (front S.\\ known')
