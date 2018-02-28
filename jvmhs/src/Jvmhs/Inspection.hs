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
import Jvmhs.Data.Class

class Inspectable a where
  classNames :: Traversal' a ClassName
  classNames _ = pure

instance Inspectable Class where
  classNames g c =
    Class
      <$> g (_className c)
      <*> g (_classSuper c)
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
      <*> pure (_fieldConstantValue f)

instance Inspectable Method where
  classNames g m =
    Method
      <$> pure (_methodAccessFlags m)
      <*> pure (_methodName m)
      <*> classNames g (_methodDescriptor m)
      <*> pure (_methodCode m)
      <*> traverse g (_methodExceptions m)

instance Inspectable BootstrapMethod where

instance Inspectable FieldDescriptor where
  classNames g fd =
    FieldDescriptor
      <$> classNames g (fieldDescriptorType fd)

instance Inspectable MethodDescriptor where
  classNames g md =
    MethodDescriptor
      <$> traverse (classNames g) (methodDescriptorArguments md)
      <*> traverse (classNames g) (methodDescriptorReturnType md)

instance Inspectable JType where
  classNames g t =
    case t of
      JTClass cn -> JTClass <$> g cn
      a -> pure a
