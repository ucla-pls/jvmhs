{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains an syntaxtic interpretation of the class
file in `Language.JVM`.

To quickly access information about the class-file use the
`jvm-binary` package instead.

-}
module Jvmhs.Data.Class
  ( -- * Data structures
    Class (..)
  , className
  , classSuper
  , classInterfaces
  , classFields
  , classMethods
  , classBootstrapMethods

  , dependencies

  , Field (..)
  , fieldAccessFlags
  , fieldName
  , fieldDescriptor
  , fieldConstantValue
  , fieldType

  , Method (..)
  , methodAccessFlags
  , methodName
  , methodDescriptor
  , methodCode
  , methodExceptions
  , methodReturnType
  , methodArgumentTypes

  -- * Helpers
  , isoBinary

  -- * Re-exports
  , module Language.JVM.Type

  -- ** Wraped Types
  , BootstrapMethod (..)
  , Constant (..)
  ) where

import qualified Language.JVM                            as B
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.ConstantValue as B
import           Language.JVM.Type

import           Control.Lens
import qualified Data.Set                                as Set
import qualified Data.Text                               as Text

newtype Constant = Constant
  { unConstant :: B.Constant B.High
  } deriving (Show, Eq)

newtype BootstrapMethod = BootstrapMethod
  { unBootstrapMethod :: B.BootstrapMethod B.High
  } deriving (Show, Eq)

-- This is the class
data Class = Class
  { _className             :: ClassName
  -- ^ the name of the class
  , _classSuper            :: ClassName
  -- ^ the name of the super class
  , _classInterfaces       :: [ ClassName ]
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: [ Field ]
  -- ^ a list of fields
  , _classMethods          :: [ Method ]
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  } deriving (Eq, Show)

-- This is the field
data Field = Field
  { _fieldAccessFlags   :: Set.Set B.FAccessFlag
  -- ^ the set of access flags
  , _fieldName          :: Text.Text
  -- ^ the name of the field
  , _fieldDescriptor    :: FieldDescriptor
  -- ^ the field type descriptor
  , _fieldConstantValue :: Maybe Constant
  -- ^ an optional constant value
  } deriving (Eq, Show)

-- This is the method
data Method = Method
  { _methodAccessFlags :: Set.Set B.MAccessFlag
  -- ^ the set of access flags
  , _methodName        :: Text.Text
  -- ^ the name of the method
  , _methodDescriptor  :: MethodDescriptor
  -- ^ the method type descriptor
  , _methodCode        :: Maybe (B.Code B.High)
  -- ^ optionally the method can contain code
  , _methodExceptions  :: [ ClassName ]
  -- ^ the method can have one or more exceptions
  } deriving (Eq, Show)

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method

-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b

-- | Get the type from a field descriptor
fieldDType :: Lens' FieldDescriptor JType
fieldDType =
  lens fieldDescriptorType (const FieldDescriptor)

-- | Get the type of field
fieldType :: Lens' Field JType
fieldType =
  fieldDescriptor . fieldDType

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [JType]
methodDArguments =
  lens methodDescriptorArguments (\md a -> md { methodDescriptorArguments = a})

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe JType)
methodDReturnType =
  lens methodDescriptorReturnType (\md a -> md { methodDescriptorReturnType = a})

-- | Get the type of field
methodArgumentTypes :: Lens' Method [JType]
methodArgumentTypes =
  methodDescriptor . methodDArguments

methodReturnType :: Lens' Method (Maybe JType)
methodReturnType =
  methodDescriptor . methodDReturnType

-- | The dependencies of a class
dependencies :: Class -> [ ClassName ]
dependencies cls =
  cls ^. classSuper : cls ^. classInterfaces

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile
  where
    fromClassFile =
      Class
      <$> B.cThisClass
      <*> B.cSuperClass
      <*> B.cInterfaces
      <*> map fromBField . B.cFields
      <*> map fromBMethod . B.cMethods
      <*> fmap BootstrapMethod . B.cBootstrapMethods

    fromBField =
      Field
      <$> B.fAccessFlags
      <*> B.fName
      <*> B.fDescriptor
      <*> fmap (Constant . B.constantValue) . B.fConstantValue

    fromBMethod =
      Method
      <$> B.mAccessFlags
      <*> B.mName
      <*> B.mDescriptor
      <*> B.mCode
      <*> B.mExceptions

    toClassFile = undefined
