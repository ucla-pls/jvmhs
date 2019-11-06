{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module : Jvmhs.Data.Type
Copyright : (c) Christian Gram Kalhauge, 2018
License  : BSD3
Maintainer : kalhauge@cs.ucla.edu

This module reexports the Types from the `jvm-binary` packages, and creates
lenses and toJSON instances for them.

This *will* create orhpaned instances, so do not import without

-}
module Jvmhs.Data.Type
  ( FromJVMBinary (..)

  -- * ClassName
  , ClassName
  , HasClassName (..)
  , dotCls
  , strCls
  , splitClassName
  , fullyQualifiedName
  , isInnerClass
  , package
  , shorthand

  -- * MethodId
  , MethodId (..)
  , HasMethodId (..)
  , methodIdName
  , methodIdDescriptor

  , MethodDescriptor (..)
  , methodDArguments
  , methodDReturnType

  , ReturnDescriptor (..)

  -- * FieldId
  , FieldId (..)
  , HasFieldId (..)
  , fieldIdName
  , fieldIdDescriptor

  , FieldDescriptor (..)
  , fieldDType

  -- * InClass and InRefType
  , InClass (..)
  , inClassNameL
  , inClassIdL

  , InRefType (..)
  , inRefTypeL
  , inRefTypeIdL
  , asInClass

  , AbsMethodId (..)
  , mkAbsMethodId
 
  , AbsFieldId (..)
  , mkAbsFieldId


  -- * Access Flags
  , MAccessFlag (..)
  , FAccessFlag (..)
  , CAccessFlag (..)
  , ICAccessFlag (..)

  -- * Re-exports
  , B.JValue (..)
  , JType (..)
  , JBaseType (..)
  , JRefType (..)
  , AsNameAndType (..)
  , WithName (..)

  , module Language.JVM.TextSerializable
  ) where

-- lens
import           Control.Lens

-- aeson
import           Data.Aeson
import           Data.Aeson.Encoding     (text)
import           Data.Aeson.TH
import           Data.Aeson.Types        (Parser)

-- cassava
import qualified Data.Csv as Csv

-- bytestring
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C

-- hashable
import           Data.Hashable

-- base
import qualified Data.Text               as Text

-- jvm-binary
import           Language.JVM.AccessFlag
import           Language.JVM.TextSerializable
import qualified Language.JVM.Constant   as B
import  Language.JVM.Type

-- * Wrap
class FromJVMBinary b n | n -> b where
  _Binary :: Iso' n b

makeWrapped ''ClassName

class HasClassName a where
  className :: Lens' a ClassName

instance HasClassName ClassName where
  className = id
  {-# INLINE className #-}

instance Hashable ClassName where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

fullyQualifiedName ::
  Iso' ClassName Text.Text
fullyQualifiedName =
   _Wrapped
{-# INLINABLE fullyQualifiedName #-}

-- | Splits a ClassName in it's components
splitClassName :: Iso' ClassName [Text.Text]
splitClassName = fullyQualifiedName . split where
  split = iso (Text.splitOn "/") (Text.intercalate "/")
{-# INLINABLE splitClassName #-}

-- | Checks if an class is an Inner Class
isInnerClass :: ClassName -> Bool
isInnerClass = Text.any (== '$') . view fullyQualifiedName

type Package = [ Text.Text ]

-- | The package name of the class name
package :: Traversal' ClassName Package
package =
  splitClassName . _init
{-# INLINABLE package #-}

-- | The shorthand name of the class name
shorthand :: Traversal' ClassName Text.Text
shorthand =
  splitClassName . _last
{-# INLINABLE shorthand #-}

instance ToJSON ClassName where
  toJSON = String . view fullyQualifiedName

instance ToJSONKey ClassName where
  toJSONKey = ToJSONKeyText f (text . f)
    where f = view fullyQualifiedName

-- * NameAndType

instance Hashable a => Hashable (B.NameAndType a) where
  hashWithSalt i (B.NameAndType a b) =
    i `hashWithSalt`
    a `hashWithSalt`
    b

ntNameL :: Lens' (NameAndType a) Text.Text
ntNameL = lens ntName (\(NameAndType _ b) a -> NameAndType a b)

ntDescriptorL :: Lens' (NameAndType a) a
ntDescriptorL = lens ntDescriptor (\(NameAndType a _)  b -> NameAndType a b)

makeWrapped ''MethodId
makeWrapped ''ReturnDescriptor

methodIdName :: Lens' MethodId Text.Text
methodIdName = _Wrapped . ntNameL
{-# INLINE methodIdName #-}

methodIdDescriptor :: Lens' MethodId MethodDescriptor
methodIdDescriptor = _Wrapped . ntDescriptorL
{-# INLINE methodIdDescriptor #-}

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [JType]
methodDArguments =
  lens methodDescriptorArguments
  (\md a -> md { methodDescriptorArguments = a })

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe JType)
methodDReturnType =
  lens methodDescriptorReturnType
  (\md a -> md { methodDescriptorReturnType = a})
  . _Wrapped

instance Hashable JBaseType where
  hashWithSalt i a =
    i `hashWithSalt` jBaseTypeToChar a

instance Hashable ReturnDescriptor where
  hashWithSalt i (ReturnDescriptor a) =
    i `hashWithSalt` a

instance Hashable MethodDescriptor where
  hashWithSalt i (MethodDescriptor a b) =
    i `hashWithSalt`
    a `hashWithSalt`
    b

class HasMethodId a where
  methodId :: Lens' a MethodId

  methodName :: Lens' a Text.Text
  methodName = methodId . methodIdName
  {-# INLINE methodName #-}

  methodDescriptor :: Lens' a MethodDescriptor
  methodDescriptor = methodId . methodIdDescriptor
  {-# INLINE methodDescriptor #-}

  -- | Get the type of field
  methodArgumentTypes :: Lens' a [JType]
  methodArgumentTypes =
    methodDescriptor . methodDArguments
  {-# INLINE methodArgumentTypes #-}

  -- | Get the return type
  methodReturnType :: Lens' a (Maybe JType)
  methodReturnType =
    methodDescriptor . methodDReturnType
  {-# INLINE methodReturnType #-}

instance HasMethodId MethodId where methodId = id

instance Hashable MethodId where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)


-- * FieldId

makeWrapped ''FieldId

fieldIdName :: Lens' FieldId Text.Text
fieldIdName = _Wrapped . ntNameL
{-# INLINE fieldIdName #-}

fieldIdDescriptor :: Lens' FieldId FieldDescriptor
fieldIdDescriptor = _Wrapped . ntDescriptorL
{-# INLINE fieldIdDescriptor #-}

-- | Get the type from a field descriptor
fieldDType :: Iso' FieldDescriptor JType
fieldDType =
  coerced
{-# INLINE fieldDType #-}

class HasFieldId a where
  fieldId :: Lens' a FieldId

  fieldName :: Lens' a Text.Text
  fieldName = fieldId . fieldIdName
  {-# INLINE fieldName #-}

  fieldDescriptor :: Lens' a FieldDescriptor
  fieldDescriptor = fieldId . fieldIdDescriptor
  {-# INLINE fieldDescriptor #-}

  -- | Get the type of field
  fieldType :: Lens' a JType
  fieldType = fieldDescriptor . fieldDType
  {-# INLINE fieldType #-}

instance HasFieldId FieldId where fieldId = id

instance Hashable FieldId where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

instance Hashable FieldDescriptor where
  hashWithSalt i (FieldDescriptor a) =
    i `hashWithSalt` a

-- * JType

instance Hashable JRefType where
  hashWithSalt i = \case
    JTArray b -> i `hashWithSalt` b
    JTClass b -> i `hashWithSalt` b

instance Hashable JType where
  hashWithSalt i = \case
    JTBase b -> i `hashWithSalt` b
    JTRef  b -> i `hashWithSalt` b


-- * InClass

inClassNameL :: Lens' (InClass a) ClassName
inClassNameL = lens inClassName (\a b -> a {inClassName = b})

inClassIdL :: Lens (InClass a) (InClass b) a b
inClassIdL = lens inClassId (\a b -> a {inClassId = b})

instance HasClassName (InClass a) where
  className = inClassNameL

-- * InRefType

inRefTypeL :: Lens' (InRefType a) JRefType
inRefTypeL = lens inRefType (\a b -> a {inRefType = b})

inRefTypeIdL :: Lens (InRefType a) (InRefType b) a b
inRefTypeIdL = lens inRefTypeId (\a b -> a {inRefTypeId = b})

asInClass :: (Profunctor p, Contravariant f)
  => Optic' p f (InRefType a) (InClass a)
asInClass = to inRefTypeAsInClass


mkAbsFieldId :: (HasClassName b, HasFieldId a) => b -> a -> AbsFieldId
mkAbsFieldId cn a = AbsFieldId $ InClass (cn^.className) (a^.fieldId)

makeWrapped ''AbsFieldId

instance HasFieldId AbsFieldId where
  fieldId = _Wrapped . inClassIdL

instance HasClassName AbsFieldId where
  className = _Wrapped . inClassNameL

instance HasFieldId (InRefType FieldId) where
  fieldId = inRefTypeIdL

mkAbsMethodId :: (HasClassName b, HasMethodId a) => b -> a -> AbsMethodId
mkAbsMethodId cn a = AbsMethodId $ InClass (cn^.className) (a^.methodId)

makeWrapped ''AbsMethodId
 
instance HasMethodId AbsMethodId where
  methodId = _Wrapped . inClassIdL

instance HasClassName AbsMethodId where
  className = _Wrapped . inClassNameL

instance HasMethodId (InRefType MethodId) where
  methodId = inRefTypeIdL

parserFromEither :: Either String a -> Parser a
parserFromEither = either error return

-- -- * Instances

instance ToJSON JType where
  toJSON = String . serialize

instance Csv.ToField AbsMethodId where
  toField = Csv.toField . serialize

instance Csv.ToField AbsFieldId where
  toField = Csv.toField . serialize

instance FromJSON JType where
  parseJSON = withText "JType" (parserFromEither . deserialize)

instance ToJSON JRefType where
  toJSON = String . serialize

instance FromJSON JRefType where
  parseJSON = withText "JRefType" (parserFromEither . deserialize)

instance ToJSON FieldId where
  toJSON = String . serialize

instance ToJSON MethodId where
  toJSON = String . serialize

instance ToJSON AbsFieldId where
  toJSON = String . serialize

instance ToJSON AbsMethodId where
  toJSON = String . serialize

instance FromJSON FieldId where
  parseJSON = withText "FieldId" (parserFromEither . deserialize)

instance FromJSON MethodId where
  parseJSON = withText "MethodId" (parserFromEither . deserialize)

instance ToJSONKey FieldId where
  toJSONKey = ToJSONKeyText serialize (text . serialize)

instance FromJSONKey FieldId where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance ToJSONKey MethodId where
  toJSONKey = ToJSONKeyText serialize (text . serialize)

instance FromJSONKey MethodId where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance FromJSON ClassName where
  parseJSON = withText "ClassName" (parserFromEither . deserialize)

instance FromJSONKey ClassName where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance ToJSON FieldDescriptor where
  toJSON = String . serialize

instance ToJSON MethodDescriptor where
  toJSON = String . serialize

instance ToJSON BS.ByteString where
  toJSON = String . Text.pack . C.unpack

instance ToJSON ReturnDescriptor where
  toJSON = String . serialize

instance ToJSON (B.MethodHandle B.High) where
  toJSON _ = String "MethodHandle"

$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''CAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''FAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''MAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''ICAccessFlag)
$(deriveToJSON (defaultOptions
                 { sumEncoding             = ObjectWithSingleField
                 , constructorTagModifier  = camelTo2 '_' . drop 1
                 }
               ) ''B.JValue)
