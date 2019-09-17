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
  , dotCls
  , strCls
  , splitClassName
  , fullyQualifiedName
  , package
  , shorthand

  -- * MethodName
  , MethodName
  , mkMethodName
  , methodNameId
  , methodNameDescriptor
  , methodId
  , methodDescriptor
  , methodArgumentTypes
  , methodReturnType
  , methodNameToText

  , MethodDescriptor
  , methodDArguments
  , methodDReturnType

  -- * FieldName
  , FieldName
  , mkFieldName
  , fieldNameId
  , fieldNameDescriptor
  , fieldType
  , fieldNameToText

  , FieldDescriptor
  , fieldDType

  -- , MethodDescriptor (..)

  -- , MethodId
  -- , mkMethodId
  -- , methodIdName
  -- , methodIdDescriptor
  -- , methodIdToText

  -- , FieldDescriptor (..)

  -- , FieldId
  -- , mkFieldId
  -- , fieldIdName
  -- , fieldIdDescriptor
  -- , fieldIdToText

  -- , InClass
  -- , inClass
  -- , inClassName
  -- , inId
  -- , inClassToText

  , AbsMethodName
  , AbsFieldName

  , B.JType (..)
  , B.JBaseType (..)
  , B.JValue (..)

  , MAccessFlag (..)
  , FAccessFlag (..)
  , CAccessFlag (..)
  , ICAccessFlag (..)

  -- , toText
  ) where

-- lens
import           Control.Lens

-- aeson
import           Data.Aeson
import           Data.Aeson.Encoding     (text)
import           Data.Aeson.TH
import           Data.Aeson.Types        (Parser)

-- deepseq
import           Control.DeepSeq

-- bytestring
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C

-- hashable
import           Data.Hashable

-- base
import           Data.String
import qualified Data.Text               as Text
import           GHC.Generics            (Generic)

-- jvm-binary
import           Language.JVM.AccessFlag
import qualified Language.JVM.Constant   as B
import qualified Language.JVM.Type       as B

-- jvmhs
import           Jvmhs.Data.Named

-- * Wrap
class FromJVMBinary b n | n -> b where
  _Binary :: Iso' n b

-- * ClassName
newtype ClassName =
  ClassName (Name (B.ClassName))
  deriving (Eq, Ord, Generic)
  deriving anyclass NFData

makeWrapped ''ClassName
makeWrapped ''B.ClassName

instance Hashable ClassName where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

instance Hashable B.ClassName where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

instance FromJVMBinary B.ClassName ClassName where
  _Binary =  _Wrapped . from asName
  {-# INLINE _Binary #-}

instance IsString ClassName where
  fromString = view $ to fromString . from _Binary

fullyQualifiedName ::
  Iso' ClassName Text.Text
fullyQualifiedName =
   _Binary . _Wrapped
{-# INLINABLE fullyQualifiedName #-}

-- | Splits a ClassName in it's components
splitClassName :: Iso' ClassName [Text.Text]
splitClassName =
  fullyQualifiedName . split
  where
    split = iso (Text.splitOn "/") (Text.intercalate "/")
{-# INLINABLE splitClassName #-}

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

dotCls :: Text.Text -> ClassName
dotCls =
  view $ to B.dotCls . from _Binary
{-# INLINABLE dotCls #-}

strCls :: String -> ClassName
strCls =
  view $ to B.strCls . from _Binary
{-# INLINABLE strCls #-}


instance Show ClassName where
  showsPrec d n =
      showsPrec d (view fullyQualifiedName n)



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

ntName :: Lens' (B.NameAndType a) Text.Text
ntName = lens B.ntName (\a b -> a { B.ntName = b })

ntDescriptor :: Lens' (B.NameAndType a) a
ntDescriptor = lens B.ntDescriptor (\a b -> a { B.ntDescriptor = b })

-- * MethodName

newtype MethodName =
  MethodName (Name (B.MethodId))
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass NFData

makeWrapped ''MethodName
makeWrapped ''B.MethodId

mkMethodName :: Text.Text -> MethodDescriptor -> MethodName
mkMethodName t d = view (from _Binary) . B.MethodId $ B.NameAndType t d

methodNameId :: Lens' MethodName Text.Text
methodNameId = _Binary . _Wrapped . ntName
{-# INLINE methodNameId #-}

methodId :: HasName MethodName e => Lens' e Text.Text
methodId = name . methodNameId
{-# INLINE methodId #-}

methodNameDescriptor :: Lens' MethodName MethodDescriptor
methodNameDescriptor = _Binary . _Wrapped . ntDescriptor
{-# INLINE methodNameDescriptor #-}

methodDescriptor :: HasName MethodName e => Lens' e MethodDescriptor
methodDescriptor = name . methodNameDescriptor
{-# INLINE methodDescriptor #-}

-- | Get the type of field
methodArgumentTypes :: HasName MethodName e => Lens' e [B.JType]
methodArgumentTypes =
  name . methodNameDescriptor . methodDArguments
{-# INLINE methodArgumentTypes #-}

-- | Get the return type
methodReturnType :: (HasName MethodName n) => Lens' n (Maybe B.JType)
methodReturnType =
  name .  methodNameDescriptor . methodDReturnType
{-# INLINE methodReturnType #-}

instance HasName MethodName MethodName where
  name = id

instance Hashable MethodName where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

type MethodDescriptor = B.MethodDescriptor

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [B.JType]
methodDArguments =
  lens B.methodDescriptorArguments
  (\md a -> md { B.methodDescriptorArguments = a })

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe B.JType)
methodDReturnType =
  lens B.methodDescriptorReturnType
  (\md a -> md { B.methodDescriptorReturnType = a})

instance Hashable B.MethodDescriptor where
  hashWithSalt i (B.MethodDescriptor a b) =
    i `hashWithSalt`
    a `hashWithSalt`
    b

instance Hashable B.MethodId where
  hashWithSalt i a =
    i `hashWithSalt` (view _Wrapped a)

instance FromJVMBinary B.MethodId MethodName where
  _Binary =  _Wrapped . from asName
  {-# INLINE _Binary #-}

instance IsString MethodName where
  fromString = view $ to fromString . from _Binary


type AbsMethodName = (ClassName, MethodName)

-- * FieldName

newtype FieldName =
  FieldName (Name (B.FieldId))
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass NFData

makeWrapped ''FieldName
makeWrapped ''B.FieldId

mkFieldName :: Text.Text -> FieldDescriptor -> FieldName
mkFieldName t d = view (from _Binary) . B.FieldId $ B.NameAndType t d

fieldNameId :: Lens' FieldName Text.Text
fieldNameId = _Binary . _Wrapped . ntName
{-# INLINE fieldNameId #-}

fieldNameDescriptor :: Lens' FieldName FieldDescriptor
fieldNameDescriptor = _Binary . _Wrapped . ntDescriptor
{-# INLINE fieldNameDescriptor #-}

-- | Get the type of field
fieldType :: HasName FieldName e => Lens' e B.JType
fieldType =
  name . fieldNameDescriptor . fieldDType

instance HasName FieldName FieldName where
  name = id

type FieldDescriptor = B.FieldDescriptor

-- | Get the type from a field descriptor
fieldDType :: Iso' FieldDescriptor B.JType
fieldDType =
  coerced
{-# INLINE fieldDType #-}

instance Hashable FieldName where
  hashWithSalt i a = i `hashWithSalt` (view _Wrapped a)

instance Hashable B.FieldDescriptor where
  hashWithSalt i (B.FieldDescriptor a) =
    i `hashWithSalt` a

instance Hashable B.FieldId where
  hashWithSalt i a =
    i `hashWithSalt` (view _Wrapped a)

instance FromJVMBinary B.FieldId FieldName where
  _Binary =  _Wrapped . from asName
  {-# INLINE _Binary #-}

instance IsString FieldName where
  fromString = view $ to fromString . from _Binary


type AbsFieldName = (ClassName, FieldName)

-- * JType

instance Hashable B.JType where
  hashWithSalt i b =
    i `hashWithSalt` (B.typeToText b)


-- -- * FieldDescriptor


-- -- fromText :: Iso' (Maybe Text.Text) (Maybe FieldDescriptor)
-- -- fromText =
-- --   iso B.fieldDescriptorFromText B.fieldDescriptorToText

-- -- * JType

-- -- * Value

-- type FieldId = B.FieldId
-- type MethodId = B.MethodId

-- type MethodHandle = B.MethodHandle High


-- methodIdName :: Lens' MethodId Text.Text
-- methodIdName =
--   lens (\(B.MethodId nt) -> B.ntName nt) (\(B.MethodId nt) a -> mkMethodId a (B.ntDescriptor nt))

-- methodIdDescriptor :: Lens' MethodId MethodDescriptor
-- methodIdDescriptor =
--   lens (\(B.MethodId nt) -> B.ntDescriptor nt) (\(B.MethodId nt) a -> mkMethodId (B.ntName nt) a)

-- mkFieldId :: Text.Text -> FieldDescriptor -> FieldId
-- mkFieldId t d = B.FieldId $ B.NameAndType t d

-- fieldIdName :: Lens' FieldId Text.Text
-- fieldIdName =
--   lens
--     (\(B.FieldId nt) -> B.ntName nt)
--     (\(B.FieldId nt) a -> mkFieldId a (B.ntDescriptor nt))

-- fieldIdDescriptor :: Lens' FieldId FieldDescriptor
-- fieldIdDescriptor =
--   lens
--     (\(B.FieldId nt) -> B.ntDescriptor nt)
--     (\(B.FieldId nt) a -> mkFieldId (B.ntName nt) a)

parseJType :: Text.Text -> Parser B.JType
parseJType e = do
  let Right x = B.typeFromText e
  return $ x

parseFieldName :: Text.Text -> Parser FieldName
parseFieldName e = do
  let Right x = B.typeFromText e
  return $ ( B.FieldId x  ^. from _Binary)

parseMethodName :: Text.Text -> Parser MethodName
parseMethodName e = do
  let Right x = B.typeFromText e
  return $ ( B.MethodId x ^. from _Binary)

-- type InClass a = B.InClass a B.High

-- inClass :: ClassName -> a -> InClass a
-- inClass = B.InClass

-- type AbsFieldId = B.AbsFieldId B.High
-- type AbsMethodId = B.AbsMethodId B.High

-- inClassName :: Lens' (InClass a) ClassName
-- inClassName = lens (\(B.InClass cn _) -> cn) (\(B.InClass _ i) cn -> inClass cn i)

-- inId :: Lens' (InClass a) a
-- inId = lens (\(B.InClass _ i) -> i) (\(B.InClass cn _) i -> inClass cn i)

-- deriving instance Ord AbsMethodId
-- deriving instance Ord AbsFieldId

-- inClassToText :: (a -> Text.Text) -> Getter (InClass a) Text.Text
-- inClassToText f = to (\x -> x^.inClassName.fullyQualifiedName <> "." <> f (x^.inId))

methodNameToText :: MethodName -> Text.Text
methodNameToText =
  view (_Binary . _Wrapped . to B.typeToText)

fieldNameToText :: FieldName -> Text.Text
fieldNameToText =
  view (_Binary . _Wrapped . to B.typeToText)

-- -- * Instances

instance ToJSON B.JType where
  toJSON = String . B.typeToText

instance FromJSON B.JType where
  parseJSON = withText "JType" parseJType

instance ToJSON FieldName where
  toJSON = String . fieldNameToText

instance ToJSON MethodName where
  toJSON = String . methodNameToText

instance FromJSON FieldName where
  parseJSON = withText "FieldName" parseFieldName

instance FromJSON MethodName where
  parseJSON = withText "MethodName" parseMethodName

instance ToJSONKey FieldName where
  toJSONKey = ToJSONKeyText fieldNameToText (text . fieldNameToText)

instance FromJSONKey FieldName where
  fromJSONKey = FromJSONKeyTextParser parseFieldName

instance ToJSONKey MethodName where
  toJSONKey = ToJSONKeyText methodNameToText (text . methodNameToText)

instance FromJSONKey MethodName where
  fromJSONKey = FromJSONKeyTextParser parseMethodName

instance FromJSON ClassName where
  parseJSON = withText "ClassName" (return . view (from fullyQualifiedName))

instance FromJSONKey ClassName where
  fromJSONKey = FromJSONKeyTextParser (return . view (from fullyQualifiedName))

instance ToJSON B.ClassName where
  toJSON = String . view _Wrapped

instance ToJSON FieldDescriptor where
  toJSON = String . B.typeToText

instance ToJSON MethodDescriptor where
  toJSON = String . B.typeToText

instance ToJSON BS.ByteString where
  toJSON = String . Text.pack . C.unpack

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
