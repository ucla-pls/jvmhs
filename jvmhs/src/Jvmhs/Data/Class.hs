{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
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

  , classAbsFieldNames
  , classAbsMethodNames

  , traverseClass


  -- *** Helpers
  , isInterface
  , dependencies
  , classField
  , classFieldList
  -- , classFieldIds
  , classMethod
  , classMethodList
  -- , classMethodIds

  -- ** Field

  , Field (..)
  , FieldContent (..)
  , fieldAccessFlags
  -- , fieldName
  -- , fieldDescriptor
  , fieldValue
  , fieldType
  -- , fieldId
  , traverseField
  -- , asFieldId

  , mapAsFieldList

  , Method (..)
  , MethodContent (..)
  , methodAccessFlags
  -- , methodCAccessFlags
  -- , methodName
  -- , methodDescriptor
  , methodCode
  -- , methodCCode
  , methodExceptions
  -- , methodCExceptions
  , methodReturnType
  , methodArgumentTypes
  -- , methodId
  , traverseMethod
  -- , asMethodId

  , mapAsMethodList

  , InnerClass (..)
  , innerClass
  , innerOuterClass
  , innerClassName
  , innerAccessFlags

  -- * Converters
  , fromClassFile
  , toClassFile
  , isoBinary

  -- , mapAsList
  -- , mapAsFieldList
  -- , mapAsMethodList

  -- ** Wraped Types
  , CAccessFlag (..)
  , MAccessFlag (..)
  , FAccessFlag (..)
  , ICAccessFlag (..)

  , BootstrapMethod (..)
  , Code (..)

  ) where

-- base
import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Word
import           GHC.Generics                            (Generic)
import           Unsafe.Coerce

-- deep-seq
import           Control.DeepSeq

-- lens
import           Control.Lens

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- text
import qualified Data.Text                               as Text

-- containers
import qualified Data.Set                                as Set

-- unordered-containers
import qualified Data.HashMap.Strict                     as HashMap
import qualified Data.HashSet                            as HashSet

-- jvm-binary
import qualified Language.JVM                            as B
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.ConstantValue    as B
import qualified Language.JVM.Attribute.EnclosingMethod  as B
import qualified Language.JVM.Attribute.Exceptions       as B
import qualified Language.JVM.Attribute.InnerClasses     as B

-- jvmhs
import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Code
import           Jvmhs.Data.Signature
import           Jvmhs.Data.Type
import           Jvmhs.Data.Named

data InnerClass = InnerClass
  { _innerClass       :: ClassName
  , _innerOuterClass  :: Maybe ClassName
  , _innerClassName   :: Maybe Text.Text
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
  , _classInterfaces       :: HashSet.HashSet ClassName
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: HashMap.HashMap FieldName FieldContent
  -- ^ a list of fields
  , _classMethods          :: HashMap.HashMap MethodName MethodContent
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  , _classSignature        :: Maybe ClassSignature
  -- ^ the class signature
  , _classEnclosingMethod  :: Maybe (ClassName, Maybe MethodName)
  -- ^ maybe an enclosing class and method
  , _classInnerClasses     :: [InnerClass]
  -- ^ a list of inner classes
  , _classVersion          :: Maybe (Word16, Word16)
  -- ^ the version of the class file
  } deriving (Eq, Show, Generic, NFData)


-- | A Field is an id and some content
newtype Field = Field (Named FieldName FieldContent)
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)

mkField :: FieldName -> FieldContent -> Field
mkField fid fc = Field $  Named fid fc

instance HasName FieldName Field where
  name = _Wrapped . name

-- | This is the field
data FieldContent = FieldContent
  { _fieldAccessFlags :: Set.Set FAccessFlag
  -- ^ the set of access flags
  , _fieldValue       :: Maybe JValue
  -- ^ an optional value
  , _fieldSignature   :: Maybe FieldSignature
  } deriving (Eq, Show, Generic, NFData)

-- | A method is an id and some content
newtype Method = Method (Named MethodName MethodContent)
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)

mkMethod :: MethodName -> MethodContent -> Method
mkMethod mid mc = Method $ Named mid mc

instance HasName MethodName Method where
  name = _Wrapped . name

-- | This is the method
data MethodContent = MethodContent
  { _methodAccessFlags :: Set.Set MAccessFlag
  -- ^ the set of access flags
  , _methodCode        :: Maybe Code
  -- ^ optionally the method can contain code
  , _methodExceptions  :: [ ClassName ]
  -- ^ the method can have one or more exceptions
  , _methodSignature   :: Maybe MethodSignature
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''Class
makeLenses ''InnerClass

makeClassy ''FieldContent
makeClassy ''MethodContent

makeWrapped ''Field
makeWrapped ''Method

instance HasName ClassName Class where
  name = className

instance HasFieldContent Field where
  fieldContent = _Wrapped . content

instance HasMethodContent Method where
  methodContent = _Wrapped . content

classAbsMethodNames :: Fold Class AbsMethodName
classAbsMethodNames =
  (selfIndex <. classMethodList.folded).withIndex.to (\(a, b) -> (a ^. name, b ^. name))

classAbsFieldNames :: Fold Class AbsFieldName
classAbsFieldNames =
  (selfIndex <. classFieldList.folded).withIndex.to (\(a, b) -> (a ^. name, b ^. name))


-- fieldId :: Lens' Field FieldId
-- fieldId = _Wrapped . _1

-- fieldContent :: Lens' Field FieldContent
-- fieldContent = _Wrapped . _2

-- fieldName :: Lens' Field Text.Text
-- fieldName = fieldId . fieldIdName

-- fieldDescriptor :: Lens' Field FieldDescriptor
-- fieldDescriptor = fieldId . fieldIdDescriptor

-- fieldAccessFlags :: Lens' Field (Set.Set FAccessFlag)
-- fieldAccessFlags = fieldContent . fieldCAccessFlags

-- fieldValue :: Lens' Field (Maybe JValue)
-- fieldValue = fieldContent . fieldCValue

-- fieldSignature :: Lens' Field (Maybe FieldSignature)
-- fieldSignature = fieldContent . fieldCSignature

-- asFieldId :: (Class, Field) -> AbsFieldId
-- asFieldId (cls, m) = inClass (cls ^.className) (m ^.fieldId)

-- methodId :: Lens' Method MethodId
-- methodId = _Wrapped . _1

-- methodContent :: Lens' Method MethodContent
-- methodContent = _Wrapped . _2

-- methodName :: Lens' Method Text.Text
-- methodName = methodId . methodIdName

-- methodDescriptor :: Lens' Method MethodDescriptor
-- methodDescriptor = methodId . methodIdDescriptor

-- methodAccessFlags :: Lens' Method (Set.Set MAccessFlag)
-- methodAccessFlags = methodContent . methodCAccessFlags

-- methodCode :: Lens' Method (Maybe Code)
-- methodCode = methodContent . methodCCode

-- methodExceptions :: Lens' Method [ClassName]
-- methodExceptions = methodContent . methodCExceptions

-- methodSignature :: Lens' Method (Maybe MethodSignature)
-- methodSignature = methodContent . methodCSignature

-- asMethodId :: (Class, Method) -> AbsMethodId
-- asMethodId (cls, m) = inClass (cls ^.className) (m ^.methodId)

traverseClass ::
  (Traversal' ClassName a)
  -> (Traversal' (Maybe ClassName) a)
  -> (Traversal' (Set.Set CAccessFlag) a)
  -> (Traversal' (HashSet.HashSet ClassName) a)
  -> (Traversal' (HashMap.HashMap FieldName FieldContent) a)
  -> (Traversal' (HashMap.HashMap MethodName MethodContent) a)
  -> (Traversal' [ BootstrapMethod ] a)
  -> (Traversal' (Maybe ClassSignature) a)
  -> (Traversal' (Maybe (ClassName, Maybe MethodName)) a)
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
  mkFieldName
    <$> (tfn g . view (name . fieldNameId) $ s)
    <*> (tfd g . view (name . fieldNameDescriptor) $ s)
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
    mkMethodName
    <$> (tfn g . view (name . methodNameId) $ s)
    <*> (tfd g . view (name . methodNameDescriptor) $ s)
  ) <*> (
    MethodContent
      <$> (taf g . view methodAccessFlags $ s)
      <*> (tc  g . view methodCode $ s)
      <*> (tex g . view methodExceptions $ s)
      <*> (ts  g . view methodSignature $ s)
    )
{-# INLINE traverseMethod #-}

-- -- | Not a true iso morphism.
-- mapAsList :: Ord a => Iso' (Map.Map a b) [(a,b)]
-- mapAsList = iso Map.toList Map.fromList

mapAsFieldList :: Lens' (HashMap.HashMap FieldName FieldContent) [Field]
mapAsFieldList = namedMapAsList . coerced

mapAsMethodList :: Lens' (HashMap.HashMap MethodName MethodContent) [Method]
mapAsMethodList = namedMapAsList . coerced

classMethodList :: Lens' Class [Method]
classMethodList = classMethods . mapAsMethodList

classFieldList :: Lens' Class [Field]
classFieldList = classFields . mapAsFieldList

classField :: FieldName -> Getter Class (Maybe Field)
classField fid = classFields . at fid . to (fmap $ mkField fid)

classMethod :: MethodName -> Getter Class (Maybe Method)
classMethod mid = classMethods . at mid . to (fmap $ mkMethod mid)

-- | The dependencies of a class
dependencies :: Class -> [ ClassName ]
dependencies cls =
  cls ^.. classSuper ._Just ++ cls ^.. classInterfaces . folded

-- | Check if a class is an interface
isInterface :: Class -> Bool
isInterface cls =
  B.CInterface `Set.member` (cls^.classAccessFlags)


instance FromJVMBinary (B.Field B.High) Field where
  _Binary =
    iso toBField fromBField
    where
      fromBField =
        mkField
        <$> ( mkFieldName
              <$> B.fName
              <*> B.fDescriptor
            )
        <*> ( FieldContent
              <$> B.fAccessFlags
              <*> (Just . B.constantValue <=< B.fConstantValue)
              <*> fmap (\(Signature a) ->
                          fromRight (error $ "could not read " ++ show a)$ fieldSignatureFromText a
                       ) . B.fSignature
            )
      toBField =
        B.Field
          <$> B.BitSet . view fieldAccessFlags
          <*> view (name.fieldNameId)
          <*> view (name.fieldNameDescriptor)
          <*> ( B.FieldAttributes
                  <$> maybe [] (:[])
                      . fmap B.ConstantValue . view fieldValue
                  <*> maybe [] (:[]) . fmap (Signature . fieldSignatureToText) . view fieldSignature
                  <*> pure [] )

instance FromJVMBinary (B.Method B.High) Method where
  _Binary = iso toBMethod fromBMethod
    where
      fromBMethod =
        mkMethod
        <$> (
          mkMethodName
          <$> B.mName
          <*> B.mDescriptor
        ) <*> (
          MethodContent
          <$> B.mAccessFlags
          <*> fmap fromBinaryCode . B.mCode
          <*> map (view (from _Binary)) . B.mExceptions
          <*> fmap (\(Signature a) -> fromRight (error $ "could not read " ++ show a) $ methodSignatureFromText a) . B.mSignature
            )

      toBMethod =
        B.Method
          <$> unsafeCoerce . (view methodAccessFlags)
          <*> (view $ name . methodNameId)
          <*> (view $ name . methodNameDescriptor)
          <*> ( B.MethodAttributes
                  <$> maybe [] (:[]) . fmap toBinaryCode . (view methodCode)
                  <*> compress (B.Exceptions . B.SizedList . map (view _Binary))
                      . view methodExceptions
                  <*> maybe [] (:[]) . fmap (Signature . methodSignatureToText) . (view methodSignature)
                  <*> pure [] )

instance FromJVMBinary (B.InnerClass B.High) InnerClass where
  _Binary = iso toBInnerClass fromBInnerClass
    where
      fromBInnerClass =
        InnerClass
          <$> view (from _Binary) . B.icClassName
          <*> fmap (view (from _Binary)) . B.icOuterClassName
          <*> B.icInnerName
          <*> B.toSet . B.icInnerAccessFlags

      toBInnerClass :: InnerClass -> B.InnerClass B.High
      toBInnerClass =
        B.InnerClass
          <$> view (innerClass . _Binary)
          <*> preview (innerOuterClass . _Just . _Binary)
          <*> _innerClassName
          <*> B.BitSet . _innerAccessFlags

asMethodMap :: Lens' [Method] (HashMap.HashMap MethodName MethodContent)
asMethodMap = coerced . from namedMapAsList

asFieldMap :: Lens' [Field] (HashMap.HashMap FieldName FieldContent)
asFieldMap = coerced . from namedMapAsList

fromClassFile :: B.ClassFile B.High -> Class
fromClassFile =
  Class
  <$> view (from _Binary) . B.cThisClass
  <*> (\x -> if B.cThisClass x == "java/lang/Object" then Nothing else Just (view (from _Binary) $ B.cSuperClass x))
  <*> B.cAccessFlags
  <*> HashSet.fromList . toListOf (folded.from _Binary) . B.cInterfaces
  <*> view asFieldMap . toListOf (folded.from _Binary) . B.cFields
  <*> view asMethodMap . toListOf (folded.from _Binary) . B.cMethods
  <*> map fromBinaryBootstrapMethod . B.cBootstrapMethods
  <*> fmap (\(Signature a) -> fromRight (error $ "could not read " ++ show a) $ classSignatureFromText a) . B.cSignature
  <*> fmap (\e -> (view (from _Binary) $ B.enclosingClassName e, fmap (view (from _Binary)) $ B.enclosingMethodName e)) . B.cEnclosingMethod
  <*> toListOf (folded.from _Binary) . B.cInnerClasses
  <*> (Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion))


toClassFile :: Class -> B.ClassFile B.High
toClassFile =
  B.ClassFile 0xCAFEBABE
    <$> fromMaybe (0 :: Word16). fmap snd . _classVersion
    <*> fromMaybe (52 :: Word16) . fmap fst . _classVersion
    <*> (pure ())
    <*> B.BitSet . _classAccessFlags

    <*> view (className._Binary)
    <*> view _Binary . fromMaybe "java/lang/Object" . _classSuper

    <*> B.SizedList . toListOf ( classInterfaces.folded._Binary )
    <*> B.SizedList . toListOf ( classFieldList.folded._Binary )
    <*> B.SizedList . toListOf ( classMethodList.folded._Binary )
    <*> ( B.ClassAttributes
            <$> compress (B.BootstrapMethods . B.SizedList)
                . map toBinaryBootstrapMethod
                . _classBootstrapMethods
            <*> maybe [] (:[]) . fmap (Signature . classSignatureToText) . _classSignature
            <*> maybe [] (:[]) . fmap (\(cn, em) -> B.EnclosingMethod (view _Binary cn) (fmap (view _Binary) em))
                               . _classEnclosingMethod
            <*> compress B.InnerClasses
                 . map (view _Binary)
                 . _classInnerClasses
            <*> pure [])

compress :: ([a] -> b) -> [a] -> [b]
compress _ [] = []
compress f as = [f as]

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''MethodContent)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''FieldContent)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''InnerClass)

-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
