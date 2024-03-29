{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD-3-Clause
Maintainer  : kalhauge@cs.ucla.edu

This module contains an syntaxtic interpretation of the class
file in `Language.JVM`.

To quickly access information about the class-file use the
`jvm-binary` package instead.
-}
module Jvmhs.Data.Class (
  -- * Class
  Class (..),

  -- ** Lenses
  className,
  classTypeParameters,
  classSuper,
  classInterfaces,
  classFields,
  classMethods,
  classAccessFlags,
  classBootstrapMethods,
  classEnclosingMethod,
  classInnerClasses,
  classAnnotations,
  classVersion,

  -- ** Helpers
  isInterface,
  classField,
  classMethod,
  classAbsFieldIds,
  classAbsMethodIds,
  reindexBootstrapMethods,

  -- * Field
  Field (..),
  fieldName,
  fieldType,
  fieldAccessFlags,
  fieldValue,
  fieldAnnotations,

  -- ** Accessors
  fieldDescriptor,

  -- * Method
  Method (..),
  methodName,
  methodParameters,
  methodReturnType,
  methodTypeParameters,
  methodAccessFlags,
  methodCode,
  methodExceptions,
  methodAnnotations,
  methodDefaultAnnotation,

  -- ** Accessors
  methodDescriptor,

  -- ** Parameters
  Parameter (..),
  parameterNameAndFlags,
  parameterType,
  parameterVisible,
  parameterAnnotations,

  -- * InnerClass
  InnerClass (..),
  innerClass,
  innerOuterClass,
  innerClassName,
  innerAccessFlags,

  -- ** accessors
  staticInnerClasses,

  -- * BootstrapMethod
  BootstrapMethod (..),
  bootstrapMethodHandle,
  bootstrapMethodArguments,

  -- ** Re-exports
  B.MethodHandle (..),
  _MHField,
  _MHMethod,
  _MHInterface,

  -- * Re-exports
  CAccessFlag (..),
  FAccessFlag (..),
  MAccessFlag (..),
  PAccessFlag (..),
  ICAccessFlag (..),
) where

-- base
import Data.Foldable as F
import Data.Word
import GHC.Generics (Generic)

-- deep-seq
import Control.DeepSeq

-- lens
import Control.Lens
import Data.Set.Lens (setOf)

-- mtl
import Control.Monad.State

-- text
import qualified Data.Text as Text

-- containers

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

-- jvm-binary
import qualified Language.JVM as B

-- jvmhs
import Jvmhs.Data.Code
import Jvmhs.Data.Identifier
import Jvmhs.Data.Type

-- | This is the class
data Class = Class
  { -- | the description of the class
    _className' :: ClassName
  , -- | access flags of the class
    _classAccessFlags :: Set.Set CAccessFlag
  , -- | the type parameters of the class
    _classTypeParameters :: [Annotated TypeParameter]
  , -- | the superclass of the class
    _classSuper :: Maybe (Annotated ClassType)
  , -- | a list of interfaces implemented by the class
    _classInterfaces :: [Annotated ClassType]
  , -- | a list of fields
    _classFields :: [Field]
  , -- | a list of methods
    _classMethods :: [Method]
  , -- | a map of bootstrap methods. Essentially, this is a list but the
    -- list might not be complete.
    _classBootstrapMethods :: IntMap.IntMap BootstrapMethod
  , -- | maybe an enclosing class and method
    _classEnclosingMethod :: Maybe (ClassName, Maybe MethodId)
  , -- | a list of inner classes
    _classInnerClasses :: [InnerClass]
  , -- | the annotations of the class
    _classAnnotations :: Annotations
  , -- | the version of the class file
    _classVersion :: Maybe (Word16, Word16)
  }
  deriving (Eq, Show, Generic, NFData)

-- | This is the field
data Field = Field
  { -- | the name of the field
    _fieldName :: !Text.Text
  , -- | the type of the field
    _fieldType :: !(Annotated Type)
  , -- | the set of access flags
    _fieldAccessFlags :: !(Set.Set FAccessFlag)
  , -- | an optional value
    _fieldValue :: !(Maybe JValue)
  , -- | the annotations of the feild
    _fieldAnnotations :: Annotations
  }
  deriving (Eq, Show, Generic, NFData)

-- | This is the method
data Method = Method
  { -- | the name of the method
    _methodName :: !Text.Text
  , -- | a method can have specific type parameters
    _methodTypeParameters :: ![Annotated TypeParameter]
  , -- | the method parameters
    _methodParameters :: ![Parameter]
  , -- | the method return type
    _methodReturnType :: !(Annotated ReturnType)
  , -- | the set of access flags
    _methodAccessFlags :: !(Set.Set MAccessFlag)
  , -- | optionally the method can contain code
    _methodCode :: !(Maybe Code)
  , -- | the method can have one or more exceptions
    _methodExceptions :: ![Annotated ThrowsType]
  , -- | the method can have annotations, these might be duplicated in the
    -- annotation for the return type.
    _methodAnnotations :: !Annotations
  , -- | in case that this method is an attribute in an annotation it
    -- is possible to assign a default value.
    _methodDefaultAnnotation :: !(Maybe AnnotationValue)
  }
  deriving (Eq, Show, Generic, NFData)

-- | A parameter
data Parameter = Parameter
  { -- | The name and access flags of the parameter as saved in the
    -- MethodParameter attribute.
    _parameterNameAndFlags :: !(Maybe (Text.Text, Set.Set PAccessFlag))
  , -- | Some parameters are not visible, this means that they are displayed
    -- in the Method Signature but is still visible in the MethodDescription.
    _parameterVisible :: Bool
  , -- | The type of the parameter, can have another (and the same)
    -- annotations as the parameter itself.
    _parameterType :: !(Annotated Type)
  , -- | The anntoations of the parameter.
    _parameterAnnotations :: !(Annotations)
  }
  deriving (Eq, Show, Generic, NFData)

-- | An inner class
data InnerClass = InnerClass
  { -- | The name of the inner class
    _innerClass :: ClassName
  , -- | The name of the other class.
    _innerOuterClass :: Maybe ClassName
  , -- | The name of the inner class, as to be prependend to the outer class.
    _innerClassName :: Maybe Text.Text
  , -- | The access flags of the inner class.
    _innerAccessFlags :: Set.Set ICAccessFlag
  }
  deriving (Eq, Show, Generic, NFData)

-- | A BootstrapMethod
data BootstrapMethod = BootstrapMethod
  { _bootstrapMethodHandle :: B.MethodHandle B.High
  , _bootstrapMethodArguments :: [JValue]
  }
  deriving (Eq, Show, Generic, NFData)

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method
makeLenses ''Parameter
makeLenses ''InnerClass
makeLenses ''BootstrapMethod

makePrisms ''B.MethodHandle

-- | A Class has a ClassName
instance HasClassName Class where
  className = className'
  {-# INLINE className #-}

-- | A Field has a 'FieldId'
instance HasFieldId Field where
  fieldId = to (\f -> mkFieldId (f ^. fieldName) (f ^. fieldDescriptor))
  {-# INLINE fieldId #-}

-- | A Method has a 'MethodId'
instance HasMethodId Method where
  methodId = to (\m -> mkMethodId (m ^. methodName) (m ^. methodDescriptor)) --
  {-# INLINE methodId #-}

-- | Fold over the absolute method ids of the class
classAbsMethodIds :: Fold Class AbsMethodId
classAbsMethodIds =
  (selfIndex <. classMethods . folded) . withIndex . to (uncurry mkAbsMethodId)

-- | Fold over the absoulte fieldIds of the class
classAbsFieldIds :: Fold Class AbsFieldId
classAbsFieldIds =
  (selfIndex <. classFields . folded) . withIndex . to (uncurry mkAbsFieldId)

-- | Choose a field in a class by a 'FieldId'
classField :: FieldId -> Getter Class (Maybe Field)
classField fid = classFields . to (find (\a -> a ^. fieldId == fid))

-- | Choose a method in a class by a 'MethodId'
classMethod :: MethodId -> Getter Class (Maybe Method)
classMethod mid = classMethods . to (find (\a -> a ^. methodId == mid))

-- | Check if a class is an interface
isInterface :: Class -> Bool
isInterface cls = B.CInterface `Set.member` (cls ^. classAccessFlags)

-- | The descriptor of a method
methodDescriptor :: Getter Method MethodDescriptor
methodDescriptor =
  to
    ( \m ->
        MethodDescriptor
          (m ^.. methodParameters . folded . parameterType . simpleType)
          (m ^. methodReturnType . simpleType . to ReturnDescriptor)
    )

-- | The descriptor of a field
fieldDescriptor :: Getter Field FieldDescriptor
fieldDescriptor = fieldType . simpleType . to FieldDescriptor

{- | The set of all static inner classes. This is important
 for assinging annotations
-}
staticInnerClasses :: [InnerClass] -> Set.Set ClassName
staticInnerClasses =
  setOf
    (folded . filtered (view $ innerAccessFlags . contains ICStatic) . innerClass)

{- | BootstrapMethods are intented to be a list, but if some of the method
 are created or removed the indicies in the code might not match.
 This function takes a Class and reindex the bootstrapmethod list and
 update the indicies in the code.
-}
reindexBootstrapMethods :: Class -> ([BootstrapMethod], Class)
reindexBootstrapMethods = runState $ do
  (indicies, bootmethods) <-
    fmap unzip . use $ classBootstrapMethods . to IntMap.toAscList
  let reindexer = IntMap.fromAscList $ zip indicies [0 ..]

  classBootstrapMethods .= IntMap.fromAscList (zip [0 ..] bootmethods)

  classMethods
    . traverse
    . methodCode
    . _Just
    . codeByteCode
    . traverse
    . byteCodeOpcode
    %= \case
      B.Invoke (B.InvkDynamic (B.InvokeDynamic i m)) ->
        B.Invoke
          ( B.InvkDynamic
              ( B.InvokeDynamic
                  (IntMap.findWithDefault i (fromIntegral i) reindexer)
                  m
              )
          )
      a -> a

  return bootmethods
