{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

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
  , classAccessFlags
  , classInterfaces
  , classFields
  , classMethods
  , classBootstrapMethods

  , dependencies

  , Field (..)
  , fieldAccessFlags
  , fieldName
  , fieldDescriptor
  , fieldValue
  , fieldType
  , toFieldID

  , Method (..)
  , methodAccessFlags
  , methodName
  , methodDescriptor
  , methodCode
  , methodExceptions
  , methodReturnType
  , methodArgumentTypes
  , toMethodID

  -- * Helpers
  , classFieldsWhere

  -- * Converters
  , fromClassFile
  , toClassFile
  , isoBinary

  -- ** Wraped Types
  , CAccessFlag (..)
  , MAccessFlag (..)
  , FAccessFlag (..)

  , BootstrapMethod (..)
  , Code (..)
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Set                                as Set
import qualified Data.Text                               as Text
import           Data.Word
import           GHC.Generics                            (Generic)
import           Unsafe.Coerce
-- import           Data.Aeson.Encoding

import qualified Language.JVM                            as B
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.ConstantValue    as B
import qualified Language.JVM.Attribute.Exceptions       as B

import           Jvmhs.Data.Type
import           Jvmhs.LensHelpers

newtype BootstrapMethod = BootstrapMethod
  { unBootstrapMethod :: B.BootstrapMethod B.High
  } deriving (Show, Eq, Generic)

newtype Code = Code
  { unCode :: B.Code B.High
  } deriving (Show, Eq, Generic)

-- This is the class
data Class = Class
  { _className             :: ClassName
  -- ^ the name of the class
  , _classSuper            :: ClassName
  -- ^ the name of the super class
  , _classAccessFlags      :: Set.Set CAccessFlag
  -- ^ access flags of the class
  , _classInterfaces       :: [ ClassName ]
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: [ Field ]
  -- ^ a list of fields
  , _classMethods          :: [ Method ]
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  } deriving (Eq, Show, Generic)

-- This is the field
data Field = Field
  { _fieldAccessFlags   :: Set.Set FAccessFlag
  -- ^ the set of access flags
  , _fieldName          :: Text.Text
  -- ^ the name of the field
  , _fieldDescriptor    :: FieldDescriptor
  -- ^ the field type descriptor
  , _fieldValue :: Maybe JValue
  -- ^ an optional value
  } deriving (Eq, Show, Generic)

-- This is the method
data Method = Method
  { _methodAccessFlags :: Set.Set MAccessFlag
  -- ^ the set of access flags
  , _methodName        :: Text.Text
  -- ^ the name of the method
  , _methodDescriptor  :: MethodDescriptor
  -- ^ the method type descriptor
  , _methodCode        :: Maybe (Code)
  -- ^ optionally the method can contain code
  , _methodExceptions  :: [ ClassName ]
  -- ^ the method can have one or more exceptions
  } deriving (Eq, Show, Generic)

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method

toFieldID :: Getter Field FieldId
toFieldID = to (\f -> fieldId (f^.fieldName) (f^.fieldDescriptor))

toMethodID :: Getter Method MethodId
toMethodID = to (\f -> methodId (f^.methodName) (f^.methodDescriptor))

-- | A traversal of all Fields that uphold some getter.
classFieldsWhere :: (Getter Field Bool) -> Traversal' Class Field
classFieldsWhere f = classFields.traverse.which f

-- | Get the type of field
fieldType :: Lens' Field JType
fieldType =
  fieldDescriptor . fieldDType

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


fromClassFile :: B.ClassFile B.High -> Class
fromClassFile =
  Class
  <$> B.cThisClass
  <*> B.cSuperClass
  <*> B.cAccessFlags
  <*> B.cInterfaces
  <*> map fromBField . B.cFields
  <*> map fromBMethod . B.cMethods
  <*> fmap BootstrapMethod . B.cBootstrapMethods
  where
    fromBField =
      Field
      <$> B.fAccessFlags
      <*> B.fName
      <*> B.fDescriptor
      <*> (preview valueFromConstant . B.constantValue <=< B.fConstantValue)

    fromBMethod =
      Method
      <$> B.mAccessFlags
      <*> B.mName
      <*> B.mDescriptor
      <*> fmap Code . B.mCode
      <*> B.mExceptions

toClassFile :: (Word16, Word16) -> Class -> B.ClassFile B.High
toClassFile (majorv, minorv) =
  B.ClassFile 0xCAFEBABE minorv majorv ()
    <$> B.BitSet . _classAccessFlags

    <*> B.RefV . _className
    <*> B.RefV . _classSuper

    <*> B.SizedList . map B.RefV . _classInterfaces
    <*> B.SizedList . map toBField . _classFields
    <*> B.SizedList . map toBMethod . _classMethods
    <*> ( B.ClassAttributes
            <$> compress (B.BootstrapMethods . B.SizedList)
                . map unBootstrapMethod
                . _classBootstrapMethods
            <*> pure [])

  where
    toBField =
      B.Field
        <$> B.BitSet . _fieldAccessFlags
        <*> B.RefV . _fieldName
        <*> B.RefV . _fieldDescriptor
        <*> ( B.FieldAttributes
                <$> maybe [] (:[])
                    . fmap (B.ConstantValue . B.DeepRef . B.RefV)
                    . fmap (review valueFromConstant) . _fieldValue
                <*> pure [])

    toBMethod =
      B.Method
        <$> unsafeCoerce . _methodAccessFlags
        <*> B.RefV . _methodName
        <*> B.RefV . _methodDescriptor
        <*> ( B.MethodAttributes
                <$> maybe [] (:[]) . fmap unCode . _methodCode
                <*> compress (B.Exceptions . B.SizedList)
                    . fmap B.RefV . _methodExceptions
                <*> pure [])

    compress :: ([a] -> b) -> [a] -> [b]
    compress _ [] = []
    compress f as = [f as]

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile (toClassFile (52,0))

instance ToJSON BootstrapMethod where
    toJSON _ = String $ "<bootstrapmethod>"

instance ToJSON Code where
    toJSON _ = String $ "<code>"

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
