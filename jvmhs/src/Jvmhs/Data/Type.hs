{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module : Jvmhs.Data.Type
Copyright : (c) Christian Gram Kalhauge, 2018
License  : MIT
Maintainer : kalhauge@cs.ucla.edu

This module reexports the Types from the `jvm-binary` packages, and creates
lenses and toJSON instances for them.

This *will* create orhpaned instances, so do not import without

-}
module Jvmhs.Data.Type
  ( ClassName
  , dotCls
  , strCls
  , splitClassName
  , fullyQualifiedName
  , package
  , shorthand

  , MethodDescriptor (..)
  , methodDArguments
  , methodDReturnType

  , MethodId
  , mkMethodId
  , methodIdName
  , methodIdDescriptor
  -- , methodIdToText

  , FieldDescriptor (..)
  , fieldDType

  , FieldId
  , mkFieldId
  , fieldIdName
  , fieldIdDescriptor
  -- , fieldIdToText

  , InClass
  , inClass
  , inClassName
  , inId
  , AbsMethodId
  , AbsFieldId

  , JType (..)
  , JValue (..)

  , MAccessFlag (..)
  , FAccessFlag (..)
  , CAccessFlag (..)
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Char
import qualified Data.Text as Text

import           Language.JVM.AccessFlag
import           Language.JVM.Constant hiding (FieldId, MethodId, MethodHandle, InClass(..), AbsMethodId, AbsFieldId)
import qualified Language.JVM.Constant as B
import           Language.JVM.Type
-- import           Language.JVM.Utils

-- * ClassName

type Package = [ Text.Text ]

makeWrapped ''ClassName

fullyQualifiedName :: Iso' ClassName Text.Text
fullyQualifiedName = _Wrapped

-- | Splits a ClassName in it's components
splitClassName :: Iso' ClassName [Text.Text]
splitClassName =
  fullyQualifiedName . split
  where
    split = iso (Text.splitOn "/") (Text.intercalate "/")

-- | The package name of the class name
package :: Traversal' ClassName Package
package =
  splitClassName . _init

-- | The shorthand name of the class name
shorthand :: Traversal' ClassName Text.Text
shorthand =
  splitClassName . _last

-- * MethodDescriptor

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [JType]
methodDArguments =
  lens
    methodDescriptorArguments
    (\md a -> md { methodDescriptorArguments = a })

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe JType)
methodDReturnType =
  lens methodDescriptorReturnType
    (\md a -> md { methodDescriptorReturnType = a})

-- * FieldDescriptor

-- | Get the type from a field descriptor
fieldDType :: Iso' FieldDescriptor JType
fieldDType =
  coerced
{-# INLINE fieldDType #-}

-- fromText :: Iso' (Maybe Text.Text) (Maybe FieldDescriptor)
-- fromText =
--   iso B.fieldDescriptorFromText B.fieldDescriptorToText

-- * JType

-- * Value

type FieldId = B.FieldId
type MethodId = B.MethodId

type MethodHandle = B.MethodHandle High

mkMethodId :: Text.Text -> MethodDescriptor -> MethodId
mkMethodId t d = B.MethodId $ B.NameAndType t d

methodIdName :: Lens' MethodId Text.Text
methodIdName =
  lens (\(B.MethodId nt) -> B.ntName nt) (\(B.MethodId nt) a -> mkMethodId a (B.ntDescriptor nt))

methodIdDescriptor :: Lens' MethodId MethodDescriptor
methodIdDescriptor =
  lens (\(B.MethodId nt) -> B.ntDescriptor nt) (\(B.MethodId nt) a -> mkMethodId (B.ntName nt) a)

mkFieldId :: Text.Text -> FieldDescriptor -> FieldId
mkFieldId t d = B.FieldId $ B.NameAndType t d

fieldIdName :: Lens' FieldId Text.Text
fieldIdName =
  lens
    (\(B.FieldId nt) -> B.ntName nt)
    (\(B.FieldId nt) a -> mkFieldId a (B.ntDescriptor nt))

fieldIdDescriptor :: Lens' FieldId FieldDescriptor
fieldIdDescriptor =
  lens
    (\(B.FieldId nt) -> B.ntDescriptor nt)
    (\(B.FieldId nt) a -> mkFieldId (B.ntName nt) a)


instance ToJSON FieldId where
  toJSON (B.FieldId f) = String . toText $ f

instance ToJSON MethodId where
  toJSON (B.MethodId m) = String . toText $ m

instance ToJSONKey FieldId where
  -- toJSON (B.FieldId f) = String . toText $ f

instance ToJSONKey MethodId where
  -- toJSON (B.MethodId m) = String . toText $ m

type InClass a = B.InClass a B.High

inClass :: ClassName -> a -> InClass a
inClass = B.InClass

type AbsFieldId = B.AbsFieldId B.High
type AbsMethodId = B.AbsMethodId B.High

inClassName :: Lens' (InClass a) ClassName
inClassName = lens (\(B.InClass cn _) -> cn) (\(B.InClass _ i) cn -> inClass cn i)

inId :: Lens' (InClass a) a
inId = lens (\(B.InClass _ i) -> i) (\(B.InClass cn _) i -> inClass cn i)

deriving instance Ord AbsMethodId
deriving instance Ord AbsFieldId

-- * Instances

instance ToJSON ClassName where
  toJSON = String . view fullyQualifiedName

instance FromJSON ClassName where
  parseJSON = withText "ClassName" (return . ClassName)

instance ToJSON FieldDescriptor where
  toJSON = String . toText

instance ToJSON MethodDescriptor where
  toJSON = String . toText

instance ToJSON BS.ByteString where
  toJSON = String . Text.pack . C.unpack

instance ToJSON MethodHandle where
  toJSON _ = String "MethodHandle"


$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''CAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''FAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''MAccessFlag)
$(deriveToJSON (defaultOptions
                 { sumEncoding             = ObjectWithSingleField
                 , constructorTagModifier  = map toLower . drop 1
                 }
               ) ''JValue)
