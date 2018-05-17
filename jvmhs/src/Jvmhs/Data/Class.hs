{-# LANGUAGE DeriveAnyClass    #-}
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
  , classSignature
  , classVersion
  , traverseClass

  , dependencies

  , Field (..)
  , fieldAccessFlags
  , fieldName
  , fieldDescriptor
  , fieldValue
  , fieldType
  , fieldSignature
  , toFieldId
  , traverseField

  , Method (..)
  , methodAccessFlags
  , methodName
  , methodDescriptor
  , methodCode
  , methodExceptions
  , methodReturnType
  , methodArgumentTypes
  , methodSignature
  , toMethodId
  , traverseMethod

  -- * Helpers
  , classField
  , classFieldsWhere
  , classMethod
  , classMethodsWhere

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

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
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
import qualified Language.JVM.Attribute.Signature       as B

import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Code
import           Jvmhs.Data.Type
import           Jvmhs.LensHelpers

-- | This is the class
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
  , _classSignature        :: Maybe Text.Text
  , _classVersion          :: Maybe (Word16, Word16)
  -- ^ the version of the class file
  } deriving (Eq, Show, Generic, NFData)

-- | This is the field
data Field = Field
  { _fieldAccessFlags :: Set.Set FAccessFlag
  -- ^ the set of access flags
  , _fieldName        :: Text.Text
  -- ^ the name of the field
  , _fieldDescriptor  :: FieldDescriptor
  -- ^ the field type descriptor
  , _fieldValue       :: Maybe JValue
  -- ^ an optional value
  , _fieldSignature   :: Maybe Text.Text
  } deriving (Eq, Show, Generic, NFData)

-- | This is the method
data Method = Method
  { _methodAccessFlags :: Set.Set MAccessFlag
  -- ^ the set of access flags
  , _methodName        :: Text.Text
  -- ^ the name of the method
  , _methodDescriptor  :: MethodDescriptor
  -- ^ the method type descriptor
  , _methodCode        :: Maybe Code
  -- ^ optionally the method can contain code
  , _methodExceptions  :: [ ClassName ]
  -- ^ the method can have one or more exceptions
  , _methodSignature   :: Maybe Text.Text
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method

traverseClass ::
  (Traversal' ClassName a)
  -> (Traversal' ClassName a)
  -> (Traversal' (Set.Set CAccessFlag) a)
  -> (Traversal' [ ClassName ] a)
  -> (Traversal' [ Field ] a)
  -> (Traversal' [ Method ] a)
  -> (Traversal' [ BootstrapMethod ] a)
  -> (Traversal' (Maybe Text.Text) a)
  -> (Traversal' Class a)
traverseClass tcn tsn taf tis tfs tms tbs tss g s =
  Class
  <$> (tcn g . _className $ s)
  <*> (tsn g . _classSuper $ s)
  <*> (taf g . _classAccessFlags $ s)
  <*> (tis g . _classInterfaces $ s)
  <*> (tfs g . _classFields $ s)
  <*> (tms g . _classMethods $ s)
  <*> (tbs g . _classBootstrapMethods $ s)
  <*> (tss g . _classSignature $ s)
  <*> (pure $ _classVersion s)
{-# INLINE traverseClass #-}

traverseField ::
  (Traversal' (Set.Set FAccessFlag) a)
  -> (Traversal' Text.Text a)
  -> (Traversal' FieldDescriptor a)
  -> (Traversal' (Maybe JValue) a)
  -> (Traversal' (Maybe Text.Text) a)
  -> Traversal' Field a
traverseField taf tfn tfd tjv ts g s =
  Field
  <$> (taf g . _fieldAccessFlags $ s)
  <*> (tfn g . _fieldName $ s)
  <*> (tfd g . _fieldDescriptor $ s)
  <*> (tjv g . _fieldValue $ s)
  <*> (ts g . _fieldSignature $ s)
{-# INLINE traverseField #-}

traverseMethod ::
  (Traversal' (Set.Set MAccessFlag) a)
  -> (Traversal' Text.Text a)
  -> (Traversal' MethodDescriptor a)
  -> (Traversal' (Maybe Code) a)
  -> (Traversal' [ClassName] a)
  -> (Traversal' (Maybe Text.Text) a)
  -> Traversal' Method a
traverseMethod taf tfn tfd tc tex ts g s =
  Method
  <$> (taf g . _methodAccessFlags $ s)
  <*> (tfn g . _methodName $ s)
  <*> (tfd g . _methodDescriptor $ s)
  <*> (tc  g . _methodCode $ s)
  <*> (tex g . _methodExceptions $ s)
  <*> (ts  g . _methodSignature $ s)
{-# INLINE traverseMethod #-}

-- | get the 'FieldId' from a 'Field'.
toFieldId :: Getter Field FieldId
toFieldId = to (\f -> B.FieldId $ B.NameAndType (f^.fieldName) (f^.fieldDescriptor))

-- | A traversal of all Fields that uphold some getter.
classFieldsWhere :: (Getter Field Bool) -> Traversal' Class Field
classFieldsWhere f = classFields.traverse.which f

-- | Get a Field where
classField :: FieldId -> Traversal' Class Field
classField fd = classFieldsWhere (toFieldId . to (==fd))

-- | get the 'MethodId' from a 'Method'.
toMethodId :: Getter Method MethodId
toMethodId = to (\f -> B.MethodId $ B.NameAndType (f^.methodName) (f^.methodDescriptor))

-- | A traversal of all Methods that uphold some getter.
classMethodsWhere :: (Getter Method Bool) -> Traversal' Class Method
classMethodsWhere f = classMethods.traverse.which f

-- | Get a Method where
classMethod :: MethodId -> Traversal' Class Method
classMethod fd = classMethodsWhere (toMethodId . to (==fd))

-- | Get the type of field
fieldType :: Lens' Field JType
fieldType =
  fieldDescriptor . fieldDType

-- | Get the type of field
methodArgumentTypes :: Lens' Method [JType]
methodArgumentTypes =
  methodDescriptor . methodDArguments

-- | Get the return type
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
  <*> B.unSizedList . B.cInterfaces
  <*> map fromBField . B.cFields
  <*> map fromBMethod . B.cMethods
  <*> map fromBinaryBootstrapMethod . B.cBootstrapMethods
  <*> fmap B.signatureToText . B.cSignature
  <*> (Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion))
  where
    fromBField =
      Field
      <$> B.fAccessFlags
      <*> B.fName
      <*> B.fDescriptor
      <*> (Just . B.constantValue <=< B.fConstantValue)
      <*> fmap B.signatureToText . B.fSignature

    fromBMethod =
      Method
      <$> B.mAccessFlags
      <*> B.mName
      <*> B.mDescriptor
      <*> fmap fromBinaryCode . B.mCode
      <*> B.mExceptions
      <*> fmap B.signatureToText . B.mSignature

toClassFile :: Class -> B.ClassFile B.High
toClassFile =
  B.ClassFile 0xCAFEBABE
    <$> fromMaybe 0 . fmap snd . _classVersion
    <*> fromMaybe 52 . fmap fst . _classVersion
    <*> (pure ())
    <*> B.BitSet . _classAccessFlags

    <*> _className
    <*> _classSuper

    <*> B.SizedList . _classInterfaces
    <*> B.SizedList . map toBField . _classFields
    <*> B.SizedList . map toBMethod . _classMethods
    <*> ( B.ClassAttributes
            <$> compress (B.BootstrapMethods . B.SizedList)
                . map toBinaryBootstrapMethod
                . _classBootstrapMethods
            <*> maybe [] (:[]) . fmap B.signatureFromText . _classSignature
            <*> pure [])

  where
    toBField =
      B.Field
        <$> B.BitSet . _fieldAccessFlags
        <*> _fieldName
        <*> _fieldDescriptor
        <*> ( B.FieldAttributes
                <$> maybe [] (:[])
                    . fmap B.ConstantValue . _fieldValue
                <*> maybe [] (:[]) . fmap B.signatureFromText . _fieldSignature
                <*> pure [] )

    toBMethod =
      B.Method
        <$> unsafeCoerce . _methodAccessFlags
        <*> _methodName
        <*> _methodDescriptor
        <*> ( B.MethodAttributes
                <$> maybe [] (:[]) . fmap toBinaryCode . _methodCode
                <*> compress (B.Exceptions . B.SizedList)
                    . _methodExceptions
                <*> maybe [] (:[]) . fmap B.signatureFromText. _methodSignature
                <*> pure [] )

    compress :: ([a] -> b) -> [a] -> [b]
    compress _ [] = []
    compress f as = [f as]

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
