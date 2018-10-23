{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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
    -- ** Class
    Class (..)
  , className
  , classSuper
  , classAccessFlags
  , classInterfaces
  , classFields
  , classMethods
  , classBootstrapMethods
  , classSignature
  , classEnclosingMethod
  , classInnerClasses
  , classVersion
  , traverseClass


  -- *** Helpers
  , isInterface
  , dependencies
  , classField
  , classFieldList
  , classFieldIds
  , classMethod
  , classMethodList
  , classMethodIds

  -- ** Field

  , Field (..)
  , FieldContent (..)
  , fieldAccessFlags
  , fieldName
  , fieldDescriptor
  , fieldValue
  , fieldType
  , fieldId
  , traverseField
  , asFieldId

  , Method (..)
  , MethodContent (..)
  , methodAccessFlags
  , methodCAccessFlags
  , methodName
  , methodDescriptor
  , methodCode
  , methodCCode
  , methodExceptions
  , methodCExceptions
  , methodReturnType
  , methodArgumentTypes
  , methodId
  , traverseMethod
  , asMethodId

  , InnerClass (..)
  , innerClass
  , innerOuterClass
  , innerName
  , innerAccessFlags

  -- * Converters
  , fromClassFile
  , toClassFile
  , isoBinary

  , mapAsList
  , mapAsFieldList
  , mapAsMethodList

  -- ** Wraped Types
  , CAccessFlag (..)
  , MAccessFlag (..)
  , FAccessFlag (..)
  , ICAccessFlag (..)

  , BootstrapMethod (..)
  , Code (..)

  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Map                                as Map
import Data.Either
import           Data.Maybe
import qualified Data.Set                                as Set
import qualified Data.Text                               as Text
import           Data.Word
import           GHC.Generics                            (Generic)
import           Unsafe.Coerce
-- import           Data.Aeson.Encoding

import qualified Language.JVM                            as B
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.EnclosingMethod  as B
import qualified Language.JVM.Attribute.InnerClasses     as B
import qualified Language.JVM.Attribute.ConstantValue    as B
import qualified Language.JVM.Attribute.Exceptions       as B

import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Code
import           Jvmhs.Data.Type
import           Jvmhs.Data.Signature

data InnerClass = InnerClass
  { _innerClass :: ClassName
  , _innerOuterClass :: Maybe ClassName
  , _innerName :: Maybe Text.Text
  , _innerAccessFlags :: Set.Set ICAccessFlag
  } deriving (Eq, Show, Generic, NFData)

-- | This is the class
data Class = Class
  { _className             :: ClassName
  -- ^ the name of the class
  , _classSuper            :: Maybe ClassName
  -- ^ the name of the super class
  , _classAccessFlags      :: Set.Set CAccessFlag
  -- ^ access flags of the class
  , _classInterfaces       :: Set.Set ClassName
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: Map.Map FieldId FieldContent
  -- ^ a list of fields
  , _classMethods          :: Map.Map MethodId MethodContent
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  , _classSignature        :: Maybe ClassSignature
  -- ^ the class signature
  , _classEnclosingMethod  :: Maybe (ClassName, Maybe MethodId)
  -- ^ maybe an enclosing class and method
  , _classInnerClasses     :: [InnerClass]
  -- ^ a list of inner classes
  , _classVersion          :: Maybe (Word16, Word16)
  -- ^ the version of the class file
  } deriving (Eq, Show, Generic, NFData)


-- | A Field is an id and some content
newtype Field = Field (FieldId, FieldContent)
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)


mkField :: FieldId -> FieldContent -> Field
mkField fid fc = Field (fid, fc)

-- | This is the field
data FieldContent = FieldContent
  { _fieldCAccessFlags :: Set.Set FAccessFlag
  -- ^ the set of access flags
  , _fieldCValue       :: Maybe JValue
  -- ^ an optional value
  , _fieldCSignature   :: Maybe FieldSignature
  } deriving (Eq, Show, Generic, NFData)

-- | A method is an id and some content
newtype Method = Method (MethodId, MethodContent)
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)


mkMethod :: MethodId -> MethodContent -> Method
mkMethod mid mc = Method (mid, mc)

-- | This is the method
data MethodContent = MethodContent
  { _methodCAccessFlags :: Set.Set MAccessFlag
  -- ^ the set of access flags
  , _methodCCode        :: Maybe Code
  -- ^ optionally the method can contain code
  , _methodCExceptions  :: [ ClassName ]
  -- ^ the method can have one or more exceptions
  , _methodCSignature   :: Maybe MethodSignature
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''Class
makeLenses ''FieldContent
makeLenses ''MethodContent
makeLenses ''InnerClass
makeWrapped ''Field
makeWrapped ''Method

classMethodIds :: Fold Class AbsMethodId
classMethodIds =
  (selfIndex <. classMethodList.folded).withIndex.to asMethodId

classFieldIds :: Fold Class AbsFieldId
classFieldIds =
  (selfIndex <. classFieldList.folded).withIndex.to asFieldId


fieldId :: Lens' Field FieldId
fieldId = _Wrapped . _1

fieldContent :: Lens' Field FieldContent
fieldContent = _Wrapped . _2

fieldName :: Lens' Field Text.Text
fieldName = fieldId . fieldIdName

fieldDescriptor :: Lens' Field FieldDescriptor
fieldDescriptor = fieldId . fieldIdDescriptor

fieldAccessFlags :: Lens' Field (Set.Set FAccessFlag)
fieldAccessFlags = fieldContent . fieldCAccessFlags

fieldValue :: Lens' Field (Maybe JValue)
fieldValue = fieldContent . fieldCValue

fieldSignature :: Lens' Field (Maybe FieldSignature)
fieldSignature = fieldContent . fieldCSignature

asFieldId :: (Class, Field) -> AbsFieldId
asFieldId (cls, m) = inClass (cls ^.className) (m ^.fieldId)

methodId :: Lens' Method MethodId
methodId = _Wrapped . _1

methodContent :: Lens' Method MethodContent
methodContent = _Wrapped . _2

methodName :: Lens' Method Text.Text
methodName = methodId . methodIdName

methodDescriptor :: Lens' Method MethodDescriptor
methodDescriptor = methodId . methodIdDescriptor

methodAccessFlags :: Lens' Method (Set.Set MAccessFlag)
methodAccessFlags = methodContent . methodCAccessFlags

methodCode :: Lens' Method (Maybe Code)
methodCode = methodContent . methodCCode

methodExceptions :: Lens' Method [ClassName]
methodExceptions = methodContent . methodCExceptions

methodSignature :: Lens' Method (Maybe MethodSignature)
methodSignature = methodContent . methodCSignature

asMethodId :: (Class, Method) -> AbsMethodId
asMethodId (cls, m) = inClass (cls ^.className) (m ^.methodId)

traverseClass ::
  (Traversal' ClassName a)
  -> (Traversal' (Maybe ClassName) a)
  -> (Traversal' (Set.Set CAccessFlag) a)
  -> (Traversal' (Set.Set ClassName) a)
  -> (Traversal' (Map.Map FieldId FieldContent) a)
  -> (Traversal' (Map.Map MethodId MethodContent) a)
  -> (Traversal' [ BootstrapMethod ] a)
  -> (Traversal' (Maybe ClassSignature) a)
  -> (Traversal' (Maybe (ClassName, Maybe MethodId)) a)
  -> (Traversal' [ InnerClass ] a)
  -> (Traversal' Class a)
traverseClass tcn tsn taf tis tfs tms tbs tss tem tin g s =
  Class
  <$> (tcn g . _className $ s)
  <*> (tsn g . _classSuper $ s)
  <*> (taf g . _classAccessFlags $ s)
  <*> (tis g . _classInterfaces $ s)
  <*> (tfs g . _classFields $ s)
  <*> (tms g . _classMethods $ s)
  <*> (tbs g . _classBootstrapMethods $ s)
  <*> (tss g . _classSignature $ s)
  <*> (tem g . _classEnclosingMethod $ s)
  <*> (tin g . _classInnerClasses $ s)
  <*> (pure $ _classVersion s)
{-# INLINE traverseClass #-}

traverseField ::
     (Traversal' Text.Text a)
  -> (Traversal' FieldDescriptor a)
  -> (Traversal' (Set.Set FAccessFlag) a)
  -> (Traversal' (Maybe JValue) a)
  -> (Traversal' (Maybe FieldSignature) a)
  -> Traversal' Field a
traverseField tfn tfd taf tjv ts g s =
  mkField
  <$> (
  mkFieldId
    <$> (tfn g . view fieldName $ s)
    <*> (tfd g . view fieldDescriptor $ s)
  ) <*> (
  FieldContent
    <$> (taf g . view fieldAccessFlags $ s)
    <*> (tjv g . view fieldValue $ s)
    <*> (ts g .  view fieldSignature $ s)
  )
{-# INLINE traverseField #-}

traverseMethod ::
     (Traversal' Text.Text a)
  -> (Traversal' MethodDescriptor a)
  -> (Traversal' (Set.Set MAccessFlag) a)
  -> (Traversal' (Maybe Code) a)
  -> (Traversal' [ClassName] a)
  -> (Traversal' (Maybe MethodSignature) a)
  -> Traversal' Method a
traverseMethod tfn tfd taf tc tex ts g s =
  mkMethod
  <$> (
    mkMethodId
    <$> (tfn g . view methodName $ s)
    <*> (tfd g . view methodDescriptor $ s)
  ) <*> (
    MethodContent
      <$> (taf g . view methodAccessFlags $ s)
      <*> (tc  g . view methodCode $ s)
      <*> (tex g . view methodExceptions $ s)
      <*> (ts  g . view methodSignature $ s)
    )
{-# INLINE traverseMethod #-}

-- | Not a true iso morphism.
mapAsList :: Ord a => Iso' (Map.Map a b) [(a,b)]
mapAsList = iso Map.toList Map.fromList

mapAsFieldList :: Lens' (Map.Map FieldId FieldContent) [Field]
mapAsFieldList = mapAsList.coerced

mapAsMethodList :: Lens' (Map.Map MethodId MethodContent) [Method]
mapAsMethodList = mapAsList.coerced

classMethodList :: Lens' Class [Method]
classMethodList = classMethods . mapAsMethodList

classFieldList :: Lens' Class [Field]
classFieldList = classFields . mapAsFieldList

classField :: FieldId -> Getter Class (Maybe Field)
classField fid = classFields . at fid . to (fmap $ mkField fid)

classMethod :: MethodId -> Getter Class (Maybe Method)
classMethod mid = classMethods . at mid . to (fmap $ mkMethod mid)

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
  cls ^.. classSuper ._Just ++ cls ^.. classInterfaces . folded

-- | Check if a class is an interface
isInterface :: Class -> Bool
isInterface cls =
  B.CInterface `Set.member` (cls^.classAccessFlags)

fromClassFile :: B.ClassFile B.High -> Class
fromClassFile =
  Class
  <$> B.cThisClass
  <*> (\x -> if B.cThisClass x == "java/lang/Object" then Nothing else Just (B.cSuperClass x))
  <*> B.cAccessFlags
  <*> Set.fromList . B.unSizedList . B.cInterfaces
  <*> Map.fromList . map fromBField . B.cFields
  <*> Map.fromList . map fromBMethod . B.cMethods
  <*> map fromBinaryBootstrapMethod . B.cBootstrapMethods
  <*> fmap (\(Signature a) -> fromRight (error $ "could not read " ++ show a) $ classSignatureFromText a) . B.cSignature
  <*> fmap (\e -> (B.enclosingClassName e, B.enclosingMethodName e)) . B.cEnclosingMethod
  <*> map fromBInnerClass . B.cInnerClasses
  <*> (Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion))
  where
    fromBField =
      (,)
      <$> ( mkFieldId
            <$> B.fName
            <*> B.fDescriptor
          )
      <*> ( FieldContent
        <$> B.fAccessFlags
        <*> (Just . B.constantValue <=< B.fConstantValue)
        <*> fmap (\(Signature a) -> fromRight (error $ "could not read " ++ show a)$ fieldSignatureFromText a) . B.fSignature
      )

    fromBMethod =
      (,)
      <$> (
        mkMethodId
        <$> B.mName
        <*> B.mDescriptor
      ) <*> (
        MethodContent
        <$> B.mAccessFlags
        <*> fmap fromBinaryCode . B.mCode
        <*> B.mExceptions
        <*> fmap (\(Signature a) -> fromRight (error $ "could not read " ++ show a) $ methodSignatureFromText a) . B.mSignature
          )

    fromBInnerClass =
      InnerClass
        <$> B.icClassName
        <*> B.icOuterClassName
        <*> B.icInnerName
        <*> B.toSet . B.icInnerAccessFlags

toClassFile :: Class -> B.ClassFile B.High
toClassFile =
  B.ClassFile 0xCAFEBABE
    <$> fromMaybe (0 :: Word16). fmap snd . _classVersion
    <*> fromMaybe (52 :: Word16) . fmap fst . _classVersion
    <*> (pure ())
    <*> B.BitSet . _classAccessFlags

    <*> _className
    <*> fromMaybe "java/lang/Object" . _classSuper

    <*> B.SizedList . Set.toList . _classInterfaces
    <*> B.SizedList . map toBField . view classFieldList
    <*> B.SizedList . map toBMethod . view classMethodList
    <*> ( B.ClassAttributes
            <$> compress (B.BootstrapMethods . B.SizedList)
                . map toBinaryBootstrapMethod
                . _classBootstrapMethods
            <*> maybe [] (:[]) . fmap (Signature . classSignatureToText) . _classSignature
            <*> maybe [] (:[]) . fmap (\(cn, em) -> B.EnclosingMethod cn em)
                               . _classEnclosingMethod
            <*> compress B.InnerClasses
                 . map toBInnerClass
                 . _classInnerClasses
            <*> pure [])

  where
    toBField =
      B.Field
        <$> B.BitSet . view fieldAccessFlags
        <*> view fieldName
        <*> view fieldDescriptor
        <*> ( B.FieldAttributes
                <$> maybe [] (:[])
                    . fmap B.ConstantValue . view fieldValue
                <*> maybe [] (:[]) . fmap (Signature . fieldSignatureToText) . view fieldSignature
                <*> pure [] )

    toBMethod =
      B.Method
        <$> unsafeCoerce . (view methodAccessFlags)
        <*> (view methodName)
        <*> (view methodDescriptor)
        <*> ( B.MethodAttributes
                <$> maybe [] (:[]) . fmap toBinaryCode . (view methodCode)
                <*> compress (B.Exceptions . B.SizedList)
                    . view methodExceptions
                <*> maybe [] (:[]) . fmap (Signature . methodSignatureToText) . (view methodSignature)
                <*> pure [] )

    toBInnerClass :: InnerClass -> B.InnerClass B.High
    toBInnerClass =
      B.InnerClass
        <$> _innerClass
        <*> _innerOuterClass
        <*> _innerName
        <*> B.BitSet . _innerAccessFlags

    compress :: ([a] -> b) -> [a] -> [b]
    compress _ [] = []
    compress f as = [f as]

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 8} ''MethodContent)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''FieldContent)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''InnerClass)


-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
