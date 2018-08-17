{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
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

import qualified Data.Set                             as Set
import qualified Data.Text                            as Text

import           Control.Lens

import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Signature
import           Jvmhs.Data.Type

import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.StackMapTable as B

class Inspectable a where
  classNames :: Traversal' a ClassName
  classNames _ = pure

  methodNames :: Traversal' a AbsMethodId
  methodNames _ = pure

nothing :: Traversal' a b
nothing = const pure
{-# INLINE nothing #-}

instance Inspectable Class where
  classNames =
    traverseClass
      id (traverse.id) nothing
      (iso Set.toList Set.fromList . traverse)
      (mapAsFieldList.traverse.classNames)
      (mapAsMethodList.traverse.classNames)
      (traverse.classNames)
      (traverse.classNames)
      (traverse.tuple id (_Just.classNames))
      (traverse.classNames)

tuple :: Traversal' a b -> Traversal' c b -> Traversal' (a,c) b
tuple fl fr g s = (,) <$> (fl g . fst $ s) <*> (fr g . snd $ s)

instance Inspectable Field where
  classNames =
    traverseField
      nothing
      classNames
      nothing
      (traverse.classNames)
      (traverse.classNames)

instance Inspectable InnerClass where
  classNames g s =
    InnerClass
      <$> (g . _innerClass $ s)
      <*> (traverse g . _innerOuterClass $ s)
      <*> (pure . _innerName $ s)
      <*> (pure . _innerAccessFlags $ s)

instance Inspectable Method where
  classNames =
    traverseMethod
      nothing
      classNames
      nothing
      (traverse.classNames)
      traverse
      (traverse.classNames)

  methodNames =
    traverseMethod
      nothing
      nothing
      nothing
      (traverse.methodNames)
      nothing
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

  methodNames =
    traverseCode
      nothing
      nothing
      (traverse.methodNames)
      nothing
      nothing

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

  methodNames g o =
    case o of
      B.Invoke r -> B.Invoke <$> methodNames g r
      _          -> pure o

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
      _            -> pure c

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

  methodNames g o =
    case o of
      B.InvkSpecial r     -> B.InvkSpecial <$> methodNames g r
      B.InvkVirtual r     -> B.InvkVirtual <$> methodNames g r
      B.InvkStatic r      -> B.InvkStatic <$> methodNames g r
      B.InvkInterface w r -> B.InvkInterface w <$> methodNames g r
      B.InvkDynamic r     -> B.InvkDynamic <$> methodNames g r

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

instance Inspectable (B.FieldId) where
  classNames g (B.FieldId d) = B.FieldId <$> classNames g d

instance Inspectable (B.MethodId) where
  classNames g (B.MethodId d) =
    B.MethodId <$> classNames g d

instance Inspectable a => Inspectable (B.NameAndType a) where
  classNames g (B.NameAndType n d) =
    B.NameAndType n <$> classNames g d

instance Inspectable (B.AbsInterfaceMethodId B.High) where
  classNames g (B.AbsInterfaceMethodId x) =
    B.AbsInterfaceMethodId <$> classNames g x

  methodNames g (B.AbsInterfaceMethodId x) =
    B.AbsInterfaceMethodId <$> methodNames g x

instance Inspectable (B.AbsMethodId B.High) where
  classNames g (B.InClass cn ci) =
    B.InClass <$> g cn <*> classNames g ci

  methodNames g a =
    g a

instance Inspectable (B.AbsVariableMethodId B.High) where
  classNames g o =
    case o of
      B.VInterfaceMethodId s -> B.VInterfaceMethodId <$> classNames g s
      B.VMethodId s          -> B.VMethodId <$> classNames g s

  methodNames g o =
    case o of
      B.VInterfaceMethodId s -> B.VInterfaceMethodId <$> methodNames g s
      B.VMethodId s          -> B.VMethodId <$> methodNames g s

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

instance Inspectable ClassSignature where
  classNames g t =
    ClassSignature
      <$> (inspectAllClassNames g . csTypeParameters) t
      <*> (classNames g . csSuperclassSignature) t
      <*> (inspectAllClassNames g . csInterfaceSignatures) t

instance Inspectable MethodSignature where
  classNames g (MethodSignature a b c d) =
    MethodSignature
      <$> inspectAllClassNames g a
      <*> inspectAllClassNames g b
      <*> inspectAllClassNames g c
      <*> inspectAllClassNames g d

instance Inspectable ThrowsSignature where
  classNames g t =
    case t of
      ThrowsClass c -> ThrowsClass <$> classNames g c
      _             -> pure t

instance Inspectable FieldSignature where
  classNames g (FieldSignature ref) =
    FieldSignature <$> classNames g ref

instance Inspectable ClassType where
  classNames g t =
    case t of
      ClassType cn ta ->
        ClassType <$> g cn <*> (traverse . traverse . classNames) g ta
      InnerClassType _ cn ta ->
        InnerClassType
           <$> (snd . Text.breakOnEnd "$" . B.classNameAsText <$> g (getClassName t))
           <*> classNames g cn
           <*> (traverse . traverse . classNames) g ta

    where
      getClassName (ClassType cn _) = cn
      getClassName (InnerClassType i oc _) =
        B.ClassName $ Text.intercalate "$" [B.classNameAsText (getClassName oc), i]

instance Inspectable TypeArgument where
  classNames g (TypeArgument i p) =
    TypeArgument i <$> classNames g p

instance Inspectable TypeParameter where
  classNames g t =
    TypeParameter
      <$> pure (tpIndentifier t)
      <*> (inspectAllClassNames g . tpClassBound) t
      <*> (inspectAllClassNames g . tpInterfaceBound) t

instance Inspectable ReferenceType where
  classNames g t =
    case t of
      RefClassType ct -> RefClassType <$> classNames g ct
      RefArrayType ct -> RefArrayType <$> classNames g ct
      _               -> pure t

instance Inspectable TypeSignature where
  classNames g t =
    case t of
      ReferenceType ct -> ReferenceType <$> classNames g ct
      _                -> pure t

inspectAllClassNames ::
  (Traversable f, Inspectable b)
  => Traversal' (f b) ClassName
inspectAllClassNames = traverse . classNames
