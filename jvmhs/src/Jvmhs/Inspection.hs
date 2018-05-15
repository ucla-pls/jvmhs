{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
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

import           Control.Lens
import           Data.Either
import qualified Data.Set                             as S

import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Type
import           Jvmhs.ClassPool

import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.StackMapTable as B

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
      nothing

instance Inspectable Field where
  classNames =
    traverseField
      nothing
      nothing
      classNames
      (traverse.classNames)
      nothing

instance Inspectable Method where
  classNames =
    traverseMethod
      nothing
      nothing
      classNames
      (traverse.classNames)
      traverse
      nothing

instance Inspectable BootstrapMethod where

instance Inspectable Code where
  classNames =
    traverseCode
      nothing
      nothing
      (traverse.classNames)
      (traverse.classNames)
      (traverse.classNames)

instance Inspectable ByteCodeOpr where
  classNames g o =
    case o of
      B.Push c            -> B.Push <$> (traverse . classNames) g c
      B.Get fa c          -> B.Get fa <$> classNames g c
      B.Put fa c          -> B.Put fa <$> classNames g c
      B.Invoke r          -> B.Invoke <$> classNames g r
      B.New c             -> B.New <$> g c
      B.NewArray c        -> B.NewArray <$> classNames g c
      B.MultiNewArray c v -> flip B.MultiNewArray v <$> g c
      B.CheckCast c       -> B.CheckCast <$> g c
      B.InstanceOf c      -> B.InstanceOf <$> g c
      _                   -> pure o

instance Inspectable ExceptionHandler where
  classNames =
    traverseExceptionHandler
      nothing
      nothing
      nothing
      (traverse.id)

instance Inspectable StackMapTable where
  classNames =
    verificationTypeInfo . classNames

instance Inspectable VerificationTypeInfo where
  classNames g c =
    case c of
      B.VTObject r -> B.VTObject <$> g r
      _           -> pure c

instance Inspectable (B.CConstant B.High) where
  classNames g o =
    case o of
      B.CRef a x -> B.CRef a <$> classNames g x
      _          -> pure o

instance Inspectable (B.Invocation B.High) where
  classNames g o =
    case o of
      B.InvkSpecial r     -> B.InvkSpecial <$> classNames g r
      B.InvkVirtual r     -> B.InvkVirtual <$> classNames g r
      B.InvkStatic r      -> B.InvkStatic <$> classNames g r
      B.InvkInterface w r -> B.InvkInterface w <$> classNames g r
      B.InvkDynamic r     -> B.InvkDynamic <$> classNames g r

instance Inspectable (B.ExactArrayType B.High) where
  classNames g a =
    case a of
      B.EARef x -> B.EARef <$> g x
      _         -> pure a

instance Inspectable (B.Constant B.High) where
  classNames g a =
    case a of
      B.CClassRef r -> B.CClassRef <$> (from fullyQualifiedName) g r
      B.CFieldRef r -> B.CFieldRef <$> classNames g r
      B.CMethodRef r -> B.CMethodRef <$> classNames g r
      B.CInterfaceMethodRef r -> B.CInterfaceMethodRef <$> classNames g r
      B.CMethodHandle r -> B.CMethodHandle <$> classNames g r
      B.CMethodType r -> B.CMethodType <$> classNames g r
      B.CInvokeDynamic r -> B.CInvokeDynamic <$> classNames g r
      _ -> pure a

instance Inspectable (B.AbsFieldId B.High) where
  classNames g (B.InClass cn ci) =
    B.InClass <$> g cn <*> classNames g ci

instance Inspectable (B.FieldId B.High) where
  classNames g (B.FieldId n d) =
    B.FieldId n <$> classNames g d

instance Inspectable (B.MethodId B.High) where
  classNames g (B.MethodId n d) =
    B.MethodId n <$> classNames g d

instance Inspectable (B.AbsInterfaceMethodId B.High) where
  classNames g (B.AbsInterfaceMethodId (B.InClass cn ci)) =
    B.AbsInterfaceMethodId <$> (B.InClass <$> g cn <*> classNames g ci)

instance Inspectable (B.AbsMethodId B.High) where
  classNames g (B.InClass cn ci) =
    B.InClass <$> g cn <*> classNames g ci

instance Inspectable (B.AbsVariableMethodId B.High) where
  classNames g o =
    case o of
      B.VInterfaceMethodId s -> B.VInterfaceMethodId <$> classNames g s
      B.VMethodId s          -> B.VMethodId <$> classNames g s

instance Inspectable (B.InvokeDynamic B.High) where
  classNames g (B.InvokeDynamic i dr) =
    B.InvokeDynamic i <$> classNames g dr

-- TODO: Implement method handle
instance Inspectable (B.MethodHandle B.High) where

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
      a          -> pure a

-- | Computes the class closure in the current space.
-- It will only include classes known to the 'MonadClassPool'.
computeClassClosure ::
  MonadClassPool m
  => S.Set ClassName
  -> m (S.Set ClassName, S.Set ClassName)
computeClassClosure =
  go (S.empty, S.empty)
  where
    go (!known, !unknown) !wave
      | S.null wave = do
        return (known, unknown)
      | otherwise = do
        -- List of all the classes that exists
        (notexists, exists) <- partitionEithers <$> wave ^!! folded . load'
        let
          found = S.fromList $ exists^..folded.className
          missed = S.fromList $ notexists^..folded.heClassName
          known' = known `S.union` found
          unknown' = unknown `S.union` missed
          front = S.fromList $ exists^..folded.classNames
        go (known', unknown') (front S.\\ known')
