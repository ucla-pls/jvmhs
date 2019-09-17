{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE RecordWildCards       #-}
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
  , classAnnotations
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
  , fieldAnnotations
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
  , methodAnnotations
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
import           Data.Foldable as F
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
import qualified Language.JVM.Attribute.Annotations      as B

-- jvmhs
import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Code
import           Jvmhs.Data.Annotation
import           Jvmhs.Data.Signature
import           Jvmhs.Data.Type
import           Jvmhs.Data.Named


data InnerClass = InnerClass
  { _innerClass       :: ClassName
  -- ^ The name of the inner class
  , _innerOuterClass  :: Maybe ClassName
  -- ^ The other class
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
  , _classAnnotations      :: Annotations
  -- ^ the annotations of the class
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
  -- ^ the signature of the field
  , _fieldAnnotations   :: Annotations
  -- ^ the annotations of the field
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
  -- ^ the signature of the method
  , _methodAnnotations  :: Annotations
  -- ^ the annotations of the method
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
  Traversal' ClassName a
  -> Traversal' (Maybe ClassName) a
  -> Traversal' (Set.Set CAccessFlag) a
  -> Traversal' (HashSet.HashSet ClassName) a
  -> Traversal' (HashMap.HashMap FieldName FieldContent) a
  -> Traversal' (HashMap.HashMap MethodName MethodContent) a
  -> Traversal' [ BootstrapMethod ] a
  -> Traversal' (Maybe ClassSignature) a
  -> Traversal' (Maybe (ClassName, Maybe MethodName)) a
  -> Traversal' [ InnerClass ] a
  -> Traversal' Annotations a
  -> Traversal' Class a
traverseClass tcn tsn taf tis tfs tms tbs tss tem tin tano g s =
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
  <*> (tano g . _classAnnotations $ s)
  <*> pure (_classVersion s)
{-# INLINE traverseClass #-}

traverseField ::
     Traversal' Text.Text a
  -> Traversal' FieldDescriptor a
  -> Traversal' (Set.Set FAccessFlag) a
  -> Traversal' (Maybe JValue) a
  -> Traversal' (Maybe FieldSignature) a
  -> Traversal' Annotations a
  -> Traversal' Field a
traverseField tfn tfd taf tjv ts tano g s =
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
    <*> (tano g .  view fieldAnnotations $ s)
  )
{-# INLINE traverseField #-}

traverseMethod ::
     Traversal' Text.Text a
  -> Traversal' MethodDescriptor a
  -> Traversal' (Set.Set MAccessFlag) a
  -> Traversal' (Maybe Code) a
  -> Traversal' [ClassName] a
  -> Traversal' (Maybe MethodSignature) a
  -> Traversal' Annotations a
  -> Traversal' Method a
traverseMethod tfn tfd taf tc tex ts tano g s =
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
      <*> (tano  g . view methodAnnotations $ s)
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
      fromBField = do
        _name <- B.fName
        _desc <- B.fDescriptor

        _fieldAccessFlags <-
          B.fAccessFlags

        _fieldValue <-
          Just . B.constantValue <=< B.fConstantValue

        _fieldSignature <-
          fmap (\(Signature a) ->
                   fromRight (error $ "could not read " ++ show a)
                   $ fieldSignatureFromText a
               )
          . B.fSignature

        _fieldAnnotations <-
          (readAnnotations <$> B.faVisibleAnnotations <*> B.faInvisibleAnnotations)
          . B.fAttributes

        pure (mkField (mkFieldName _name _desc) (FieldContent {..}))

      toBField =
        B.Field
          <$> B.BitSet . view fieldAccessFlags
          <*> view (name.fieldNameId)
          <*> view (name.fieldNameDescriptor)
          <*> ( do
                 faConstantValues <-
                   maybeToList . fmap B.ConstantValue
                   . view fieldValue

                 faSignatures <-
                   maybeToList . fmap (Signature . fieldSignatureToText)
                   . view fieldSignature

                 faVisibleAnnotations <-
                   compress (B.RuntimeVisibleAnnotations . B.SizedList)
                   . view (fieldAnnotations . visibleAnnotations)

                 faInvisibleAnnotations <-
                   compress (B.RuntimeInvisibleAnnotations . B.SizedList)
                   . view (fieldAnnotations . invisibleAnnotations)

                 faInvisibleTypeAnnotations <- pure []
                 faVisibleTypeAnnotations <- pure []
                 faOthers <- pure []

                 pure $ (B.FieldAttributes {..} :: B.FieldAttributes B.High)
             )

instance FromJVMBinary (B.Method B.High) Method where
  _Binary = iso toBMethod fromBMethod
    where
      fromBMethod = do
        _name <- B.mName
        _desc <- B.mDescriptor

        _methodAccessFlags <-
          B.mAccessFlags

        _methodCode <-
          fmap fromBinaryCode . B.mCode

        _methodExceptions <-
          map (view (from _Binary)) . B.mExceptions

        _methodSignature <-
          fmap (\(Signature a) ->
                  fromRight (error $ "could not read " ++ show a)
                  $ methodSignatureFromText a
               )
          . B.mSignature

        _methodAnnotations <-
          (readAnnotations <$> B.maVisibleAnnotations <*> B.maInvisibleAnnotations)
          . B.mAttributes

        pure $ mkMethod
          (mkMethodName _name _desc)
          (MethodContent {..})

      toBMethod =
        B.Method
          <$> unsafeCoerce . view methodAccessFlags
          <*> view (name . methodNameId)
          <*> view (name . methodNameDescriptor)
          <*> ( do
                  maCode <-
                    maybeToList . fmap toBinaryCode . view methodCode

                  maVisibleAnnotations <-
                    compress (B.RuntimeVisibleAnnotations . B.SizedList)
                    . view (methodAnnotations . visibleAnnotations)

                  maInvisibleAnnotations <-
                    compress (B.RuntimeInvisibleAnnotations . B.SizedList)
                    . view (methodAnnotations . invisibleAnnotations)

                  maExceptions <-
                    compress (B.Exceptions . B.SizedList . map (view _Binary))
                    . view methodExceptions

                  maSignatures <-
                    maybeToList . fmap (Signature . methodSignatureToText)
                    . view methodSignature

                  maAnnotationDefault <- pure []

                  maVisibleParameterAnnotations <- pure []
                  maInvisibleParamterAnnotations <- pure []
                  maVisibleTypeAnnotations <- pure []
                  maInvisibleTypeAnnotations <- pure []
                  maOthers <- pure []

                  pure $ B.MethodAttributes {..}
             )

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
fromClassFile = do
  _className <-
    view (from _Binary) . B.cThisClass

  _classSuper <-
    (\x -> if B.cThisClass x == "java/lang/Object"
      then Nothing
      else Just (view (from _Binary) $ B.cSuperClass x)
    )

  _classAccessFlags <-
    B.cAccessFlags

  _classInterfaces <-
    HashSet.fromList . toListOf (folded.from _Binary) . B.cInterfaces

  _classFields <-
    view asFieldMap . toListOf (folded.from _Binary) . B.cFields

  _classMethods <-
    view asMethodMap . toListOf (folded.from _Binary) . B.cMethods

  _classBootstrapMethods <-
    map fromBinaryBootstrapMethod . B.cBootstrapMethods

  _classSignature <-
    fmap (\(Signature a) ->
            fromRight (error $ "could not read " ++ show a) $ classSignatureFromText a
         ) . B.cSignature

  _classEnclosingMethod <-
    fmap (\e ->
            ( view (from _Binary) $ B.enclosingClassName e
            , view (from _Binary) <$> B.enclosingMethodName e
            )
         ) . B.cEnclosingMethod

  _classInnerClasses <-
    toListOf (folded.from _Binary) . B.cInnerClasses

  _classAnnotations <-
    (readAnnotations <$> B.caVisibleAnnotations <*> B.caInvisibleAnnotations)
    . B.cAttributes

  _classVersion  <-
    Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion)

  pure $ Class {..}

readAnnotations ::
  [B.RuntimeVisibleAnnotations B.High] ->
  [B.RuntimeInvisibleAnnotations B.High] -> Annotations
readAnnotations vis invis =
  Annotations
  (concatMap (toList . B.asListOfRuntimeVisibleAnnotations) $ vis)
  (concatMap (toList . B.asListOfRuntimeInvisibleAnnotations) $ invis)

toClassFile :: Class -> B.ClassFile B.High
toClassFile =
  B.ClassFile 0xCAFEBABE
    <$> maybe (0 :: Word16) snd . _classVersion
    <*> maybe (52 :: Word16) fst . _classVersion
    <*> pure ()
    <*> B.BitSet . _classAccessFlags

    <*> view (className._Binary)
    <*> view _Binary . fromMaybe "java/lang/Object" . _classSuper

    <*> B.SizedList . toListOf ( classInterfaces.folded._Binary )
    <*> B.SizedList . toListOf ( classFieldList.folded._Binary )
    <*> B.SizedList . toListOf ( classMethodList.folded._Binary )
    <*> ( do
           caBootstrapMethods <-
             compress (B.BootstrapMethods . B.SizedList)
             . map toBinaryBootstrapMethod
             . view classBootstrapMethods

           caSignature <-
             maybeToList
             . fmap (Signature . classSignatureToText)
             . view classSignature

           caEnclosingMethod <-
             maybeToList
             . fmap (\(cn, em) ->
                       B.EnclosingMethod (view _Binary cn) (fmap (view _Binary) em))
             . view classEnclosingMethod

           caInnerClasses <-
             compress B.InnerClasses
             . map (view _Binary)
             . view classInnerClasses

           caVisibleAnnotations <-
             compress (B.RuntimeVisibleAnnotations . B.SizedList)
             . view (classAnnotations . visibleAnnotations)

           caInvisibleAnnotations <-
             compress (B.RuntimeInvisibleAnnotations . B.SizedList)
             . view (classAnnotations . invisibleAnnotations)

           caVisibleTypeAnnotations <- pure []
           caInvisibleTypeAnnotations <- pure []
           caOthers <- pure []

           pure $ B.ClassAttributes {..}
       )

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
