{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE LambdaCase        #-}
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
License     : BSD-3-Clause
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
  , classTypeName
  , classAbsFieldIds
  , classAbsMethodIds

  -- *** Helpers
  , isInterface
  , dependencies
  , classField
  , classMethod

  -- ** Field

  , Field (..)
  , fieldId

  -- ** Method
  , Method (..)
  , methodId
  , methodReturnType
  , methodArgumentTypes

  , InnerClass (..)
  , innerClass
  , innerOuterClass
  , innerClassName
  , innerAccessFlags

  -- * Converters
  -- , fromClassFile
  -- , toClassFile
  -- , isoBinary

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
import           Control.Lens hiding ((.=))

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- text
import qualified Data.Text                               as Text

-- containers
import qualified Data.Set                                as Set

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

-- | This is the class
data Class = Class
  { _classDesc             :: ClassName
  -- ^ the description of the class
  , _classAccessFlags      :: Set.Set CAccessFlag
  -- ^ access flags of the class
  , _classTypeParameters   :: [ TypeParameter ]
  -- ^ the type parameters of the class
  , _classSuper            :: Maybe ClassType
  -- ^ the superclass of the class
  , _classInterfaces       :: [ ClassType ]
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: [ Field ]
  -- ^ a list of fields
  , _classMethods          :: [ Method ]
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  , _classEnclosingMethod  :: Maybe (ClassName, Maybe MethodId)
  -- ^ maybe an enclosing class and method
  , _classInnerClasses     :: [ InnerClass ]
  -- ^ a list of inner classes
  , _classAnnotations      :: Annotations
  -- ^ the annotations of the class
  , _classVersion          :: Maybe (Word16, Word16)
  -- ^ the version of the class file
  } deriving (Eq, Show, Generic, NFData)

-- | This is the field
data Field = Field
  { _fieldDesc          :: FieldId
  -- ^ the description of the field
  , _fieldAccessFlags :: Set.Set FAccessFlag
  -- ^ the set of access flags
  , _fieldValue       :: Maybe JValue
  -- ^ an optional value
  , _fieldSignature   :: Maybe FieldSignature
  -- ^ the signature of the field
  , _fieldAnnotations   :: Annotations
  -- ^ the annotations of the field
  } deriving (Eq, Show, Generic, NFData)

-- | This is the method
data Method = Method
  { _methodDesc            :: MethodId
  -- ^ the description of the method
  , _methodAccessFlags     :: Set.Set MAccessFlag
  -- ^ the set of access flags
  , _methodCode            :: Maybe Code
  -- ^ optionally the method can contain code
  , _methodExceptions      :: [ ThrowsSignature ]
  -- ^ the method can have one or more exceptions
  , _methodTypeParameters  :: [TypeParameter]
  -- ^ a method can have specific type parameters
  , _methodArguments       :: [TypeSignature]
  -- ^ the arguments of method
  , _methodReturn          :: Maybe TypeSignature
  -- ^ the return type of the method
  , _methodAnnotations     :: Annotations
  -- ^ the annotations of the method
  } deriving (Eq, Show, Generic, NFData)

-- | An inner class
data InnerClass = InnerClass
  { _innerClass       :: ClassName
  -- ^ The name of the inner class
  , _innerOuterClass  :: Maybe ClassName
  -- ^ The name of the other class.
  , _innerClassName   :: Maybe Text.Text
  -- ^ The name of the inner class, as to be prependend to the
  -- outer class.
  , _innerAccessFlags :: Set.Set ICAccessFlag
  -- ^ The access flags of the inner class.
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''InnerClass

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method

-- | A Class has a ClassName
instance HasClassName Class where
  className = classDesc
  {-# INLINE className #-}

-- | A Field has a 'FieldId'
instance HasFieldId Field where
  fieldId = fieldDesc
  {-# INLINE fieldId #-}

-- | A Methhi has a 'MethodId'
instance HasMethodId Method where
  methodId = methodDesc
  {-# INLINE methodId #-}


classAbsMethodIds :: Fold Class AbsMethodId
classAbsMethodIds =
  (selfIndex <. classMethods.folded)
  . withIndex
  . to (uncurry mkAbsMethodId)

classAbsFieldIds :: Fold Class AbsFieldId
classAbsFieldIds =
  (selfIndex <. classFields.folded)
  . withIndex
  . to (uncurry mkAbsFieldId)

classField :: FieldId -> Getter Class (Maybe Field)
classField fid =
  classFields . to (find (\a -> a ^. fieldId == fid))

classMethod :: MethodId -> Getter Class (Maybe Method)
classMethod mid =
  classMethods . to (find (\a -> a ^. methodId == mid))

-- | The dependencies of a class
dependencies :: Class -> [ ClassName ]
dependencies =
  toListOf $ (classSuper._Just <> classInterfaces.folded).classTypeName

-- | Check if a class is an interface
isInterface :: Class -> Bool
isInterface cls =
  B.CInterface `Set.member` (cls^.classAccessFlags)

-- instance FromJVMBinary (B.Field B.High) Field where
--   _Binary =
--     iso toBField fromBField
--     where
--       fromBField = do
--         _name <- B.fName
--         _desc <- B.fDescriptor
--
--         _fieldAccessFlags <-
--           B.fAccessFlags
--
--         _fieldValue <-
--           Just . B.constantValue <=< B.fConstantValue
--
--         _fieldSignature <-
--           fmap (\(Signature a) ->
--                    fromRight (error $ "could not read " ++ show a)
--                    $ fieldSignatureFromText a
--                )
--           . B.fSignature
--
--         _fieldAnnotations <-
--           (readAnnotations <$> B.faVisibleAnnotations <*> B.faInvisibleAnnotations)
--           . B.fAttributes
--
--         pure (mkField (_name <:> _desc) (FieldContent {..}))
--
--       toBField =
--         B.Field
--           <$> B.BitSet . view fieldAccessFlags
--           <*> view fieldName
--           <*> view fieldDescriptor
--           <*> ( do
--                  faConstantValues <-
--                    maybeToList . fmap B.ConstantValue
--                    . view fieldValue
--
--                  faSignatures <-
--                    maybeToList . fmap (Signature . fieldSignatureToText)
--                    . view fieldSignature
--
--                  faVisibleAnnotations <-
--                    compress (B.RuntimeVisibleAnnotations . B.SizedList)
--                    . view (fieldAnnotations . visibleAnnotations)
--
--                  faInvisibleAnnotations <-
--                    compress (B.RuntimeInvisibleAnnotations . B.SizedList)
--                    . view (fieldAnnotations . invisibleAnnotations)
--
--                  faInvisibleTypeAnnotations <- pure []
--                  faVisibleTypeAnnotations <- pure []
--                  faOthers <- pure []
--
--                  pure (B.FieldAttributes {..} :: B.FieldAttributes B.High)
--              )
--
-- instance FromJVMBinary (B.Method B.High) Method where
--   _Binary = iso toBMethod fromBMethod
--     where
--       fromBMethod :: B.Method B.High -> Method
--       fromBMethod = do
--         _name <- B.mName
--         _desc <- B.mDescriptor
--
--         _methodAccessFlags <-
--           B.mAccessFlags
--
--         _methodCode <-
--           fmap fromBinaryCode . B.mCode
--
--         _defaultMethodExceptions <-
--           B.mExceptions
--
--         _methodSignature <-
--           fmap (\(Signature a) ->
--                   fromRight (error $ "could not read " ++ show a)
--                   $ methodSignatureFromText a
--                )
--           . B.mSignature
--
--         _methodAnnotations <-
--           (readAnnotations <$> B.maVisibleAnnotations <*> B.maInvisibleAnnotations)
--           . B.mAttributes
--
--         pure $ do
--           let
--             -- TODO: A method signature encoded by the Signature attribute may not
--             -- correspond exactly to the method descriptor in the method_info
--             -- structure (§4.3.3). In particular, there is no assurance that the
--             -- number of formal parameter types in the method signature is the same
--             -- as the number of parameter descriptors in the method descriptor. The
--             -- numbers are the same for most methods, but certain constructors in
--             -- the Java programming language have an implicitly declared parameter
--             -- which a compiler represents with a parameter descriptor but may omit
--             -- from the method signature. See the note in §4.7.18 for a similar
--             -- situation involving parameter annotations.
--
--
--
--             _methodExceptions =
--               zipWith (\a -> \case Just b -> b; Nothing -> throwsSignatureFromName a)
--               _defaultMethodExceptions
--               (map Just (maybe [] msThrows _methodSignature) ++ repeat Nothing)
--
--             _methodTypeParameters =
--               maybe [] msTypeParameters _methodSignature
--
--             _methodArguments =
--               maybe (map typeSignatureFromType . methodDescriptorArguments $ _desc)
--               msArguments _methodSignature
--
--             _methodReturn =
--               maybe (fmap typeSignatureFromType
--                      . asMaybeJType . methodDescriptorReturnType
--                      $ _desc)
--               msResults _methodSignature
--
--           mkMethod
--             (_name <:> _desc)
--             (MethodContent {..})
--
--
--       methodSignature :: Method -> Maybe MethodSignature
--       methodSignature = do
--         msTypeParameters <- view methodTypeParameters
--         msArguments <- view methodArguments
--         msResults <- view methodReturn
--         msThrows <- view methodExceptions
--         pure $ do
--           let x = MethodSignature {..}
--           if isSimpleMethodSignature x
--             then Nothing
--             else Just x
--
--       toBMethod :: Method -> B.Method B.High
--       toBMethod =
--         B.Method
--           <$> unsafeCoerce . view methodAccessFlags
--           <*> view methodName
--           <*> view methodDescriptor
--           <*> ( do
--                   maCode <-
--                     maybeToList . fmap toBinaryCode . view methodCode
--
--                   maVisibleAnnotations <-
--                     compress (B.RuntimeVisibleAnnotations . B.SizedList)
--                     . view (methodAnnotations . visibleAnnotations)
--
--                   maInvisibleAnnotations <-
--                     compress (B.RuntimeInvisibleAnnotations . B.SizedList)
--                     . view (methodAnnotations . invisibleAnnotations)
--
--                   maExceptions <-
--                     compress (B.Exceptions . B.SizedList)
--                     . toListOf (methodExceptions.folded.throwsSignatureName)
--
--                   maSignatures <-
--                     maybeToList . fmap (Signature . methodSignatureToText)
--                     . methodSignature
--
--                   maAnnotationDefault <- pure []
--
--                   maVisibleParameterAnnotations <- pure []
--                   maInvisibleParamterAnnotations <- pure []
--                   maVisibleTypeAnnotations <- pure []
--                   maInvisibleTypeAnnotations <- pure []
--                   maOthers <- pure []
--
--                   pure $ B.MethodAttributes {..}
--              )
--
-- instance FromJVMBinary (B.InnerClass B.High) InnerClass where
--   _Binary = iso toBInnerClass fromBInnerClass
--     where
--       fromBInnerClass =
--         InnerClass
--           <$> B.icClassName
--           <*> B.icOuterClassName
--           <*> B.icInnerName
--           <*> B.toSet . B.icInnerAccessFlags
--
--       toBInnerClass :: InnerClass -> B.InnerClass B.High
--       toBInnerClass =
--         B.InnerClass
--           <$> view innerClass
--           <*> preview (innerOuterClass . _Just)
--           <*> _innerClassName
--           <*> B.BitSet . _innerAccessFlags

{-
 -fromClassFile :: B.ClassFile B.High -> Class
 -fromClassFile = do
 -  _className <- B.cThisClass
 -
 -  classSignature <-
 -    fmap (\(Signature a) ->
 -            fromRight (error $ "could not read " ++ show a) $ classSignatureFromText a
 -         ) . B.cSignature
 -
 -  let _classTypeParameters = concatMap csTypeParameters classSignature
 -
 -  _classSuper <- \cls ->
 -    case fmap csSuperclassSignature classSignature of
 -      _ | B.cThisClass cls == "java/lang/Object" ->
 -          Nothing
 -      Just sp
 -        -> Just sp
 -      Nothing
 -        ->
 -          Just . classTypeFromName $ B.cSuperClass cls
 -
 -  _classAccessFlags <-
 -    B.cAccessFlags
 -
 -  _classInterfaces <- \cls ->
 -    maybe [ classTypeFromName i | i <- toListOf (folding B.cInterfaces) cls ]
 -      csInterfaceSignatures classSignature
 -
 -  _classFields <-
 -    toListOf (folded.from _Binary) . B.cFields
 -
 -  _classMethods <-
 -    toListOf (folded.from _Binary) . B.cMethods
 -
 -  _classBootstrapMethods <-
 -    map fromBinaryBootstrapMethod . B.cBootstrapMethods
 -
 -  _classEnclosingMethod <-
 -    fmap (\e ->
 -            ( B.enclosingClassName e
 -            , B.enclosingMethodName e
 -            )
 -         ) . B.cEnclosingMethod
 -
 -  _classInnerClasses <-
 -    toListOf (folded.from _Binary) . B.cInnerClasses
 -
 -  _classAnnotations <-
 -    (readAnnotations <$> B.caVisibleAnnotations <*> B.caInvisibleAnnotations)
 -    . B.cAttributes
 -
 -  _classVersion  <-
 -    Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion)
 -
 -  pure $ Class (Named _className (ClassContent {..}))
 -}

readAnnotations ::
  [B.RuntimeVisibleAnnotations B.High] ->
  [B.RuntimeInvisibleAnnotations B.High] -> Annotations
readAnnotations vis invis =
  Annotations
  (concatMap (toList . B.asListOfRuntimeVisibleAnnotations) vis)
  (concatMap (toList . B.asListOfRuntimeInvisibleAnnotations) invis)

{-
 -toClassFile :: Class -> B.ClassFile B.High
 -toClassFile =
 -
 -  B.ClassFile 0xCAFEBABE
 -    <$> maybe (0 :: Word16) snd . view classVersion
 -    <*> maybe (52 :: Word16) fst . view classVersion
 -    <*> pure ()
 -    <*> B.BitSet . view classAccessFlags
 -
 -    <*> view className
 -    <*> maybe "java/lang/Object" (view classTypeName)
 -      . view classSuper
 -
 -    <*> B.SizedList . toListOf ( classInterfaces.folded.classTypeName)
 -    <*> B.SizedList . toListOf ( classFields.folded._Binary )
 -    <*> B.SizedList . toListOf ( classMethods.folded._Binary )
 -    <*> ( do
 -           caBootstrapMethods <-
 -             compress (B.BootstrapMethods . B.SizedList)
 -             . map toBinaryBootstrapMethod
 -             . view classBootstrapMethods
 -
 -           caSignature <-
 -             maybeToList
 -             . fmap (Signature . classSignatureToText)
 -             . (\cls ->
 -                 case cls^.classSuper of
 -                   Just x ->
 -                     Just $ ClassSignature (cls^.classTypeParameters) x (cls^.classInterfaces)
 -                   Nothing ->
 -                     Nothing
 -               )
 -
 -           caEnclosingMethod <-
 -             maybeToList
 -             . fmap (uncurry B.EnclosingMethod)
 -             . view classEnclosingMethod
 -
 -           caInnerClasses <-
 -             compress B.InnerClasses
 -             . map (view _Binary)
 -             . view classInnerClasses
 -
 -           caVisibleAnnotations <-
 -             compress (B.RuntimeVisibleAnnotations . B.SizedList)
 -             . view (classAnnotations . visibleAnnotations)
 -
 -           caInvisibleAnnotations <-
 -             compress (B.RuntimeInvisibleAnnotations . B.SizedList)
 -             . view (classAnnotations . invisibleAnnotations)
 -
 -           caVisibleTypeAnnotations <- pure []
 -           caInvisibleTypeAnnotations <- pure []
 -           caOthers <- pure []
 -
 -           pure $ B.ClassAttributes {..}
 -       )
 -}

compress :: ([a] -> b) -> [a] -> [b]
compress f = \case
  [] -> []
  as -> [f as]

{-
 --- | An Isomorphism between classfiles and checked classes.
 -isoBinary :: Iso' (B.ClassFile B.High) Class
 -isoBinary =
 -  iso fromClassFile toClassFile
 -}

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''InnerClass)

-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)


-- traverseClass ::
--   Traversal' ClassName a
--   -> Traversal' (Maybe ClassType) a
--   -> Traversal' (Set.Set CAccessFlag) a
--   -> Traversal' [ TypeParameter ] a
--   -> Traversal' [ ClassType ] a
--   -> Traversal' [ Field ] a
--   -> Traversal' [ Method ] a
--   -> Traversal' [ BootstrapMethod ] a
--   -> Traversal' (Maybe (ClassName, Maybe MethodId)) a
--   -> Traversal' [ InnerClass ] a
--   -> Traversal' Annotations a
--   -> Traversal' Class a
-- traverseClass tcn tsn taf ttp tis tfs tms tbs tem tin tano g s =
--   mkClass
--   <$> (tcn g . view className $ s)
--   <*>
--   ( ClassContent
--     <$> (taf g . view classAccessFlags $ s)
--     <*> (ttp g . view classTypeParameters $ s)
--     <*> (tsn g . view classSuper $ s)
--     <*> (tis g . view classInterfaces $ s)
--     <*> (tfs g . view classFields $ s)
--     <*> (tms g . view classMethods $ s)
--     <*> (tbs g . view classBootstrapMethods $ s)
--     <*> (tem g . view classEnclosingMethod $ s)
--     <*> (tin g . view classInnerClasses $ s)
--     <*> (tano g . view classAnnotations $ s)
--     <*> pure (view classVersion s)
--   )
-- {-# INLINE traverseClass #-}
--
-- traverseField ::
--      Traversal' Text.Text a
--   -> Traversal' FieldDescriptor a
--   -> Traversal' (Set.Set FAccessFlag) a
--   -> Traversal' (Maybe JValue) a
--   -> Traversal' (Maybe FieldSignature) a
--   -> Traversal' Annotations a
--   -> Traversal' Field a
-- traverseField tfn tfd taf tjv ts tano g s =
--   mkField
--   <$> (
--   (<:>)
--     <$> (tfn g . view fieldName $ s)
--     <*> (tfd g . view fieldDescriptor $ s)
--   ) <*> (
--   FieldContent
--     <$> (taf g . view fieldAccessFlags $ s)
--     <*> (tjv g . view fieldValue $ s)
--     <*> (ts g .  view fieldSignature $ s)
--     <*> (tano g .  view fieldAnnotations $ s)
--   )
-- {-# INLINE traverseField #-}
--
-- traverseMethod ::
--      Traversal' Text.Text a
--   -> Traversal' MethodDescriptor a
--   -> Traversal' (Set.Set MAccessFlag) a
--   -> Traversal' (Maybe Code) a
--   -> Traversal' [ ThrowsSignature ] a
--   -> Traversal' [ TypeParameter ] a
--   -> Traversal' [ TypeSignature ] a
--   -> Traversal' (Maybe TypeSignature) a
--   -> Traversal' Annotations a
--   -> Traversal' Method a
-- traverseMethod tfn tfd taf tc tex tp targ tret tano g s =
--   mkMethod
--   <$> (
--     (<:>)
--     <$> (tfn g . view methodName $ s)
--     <*> (tfd g . view methodDescriptor $ s)
--   ) <*> (
--     MethodContent
--       <$> (taf g . view methodAccessFlags $ s)
--       <*> (tc  g . view methodCode $ s)
--       <*> (tex g . view methodExceptions $ s)
--       <*> (tp  g . view methodTypeParameters $ s)
--       <*> (targ  g . view methodArguments $ s)
--       <*> (tret  g . view methodReturn $ s)
--       <*> (tano  g . view methodAnnotations $ s)
--     )
-- {-# INLINE traverseMethod #-}

-- -- | Not a true iso morphism.
-- mapAsList :: Ord a => Iso' (Map.Map a b) [(a,b)]
-- mapAsList = iso Map.toList Map.fromList

-- mapAsFieldList :: Lens' (HashMap.HashMap FieldId FieldContent) [Field]
-- mapAsFieldList = namedMapAsList . coerced

-- mapAsMethodList :: Lens' (HashMap.HashMap MethodId MethodContent) [Method]
-- mapAsMethodList = namedMapAsList . coerced

-- classMethodList :: Lens' Class [Method]
-- classMethodList = classMethods . mapAsMethodList

-- classFieldList :: Lens' Class [Field]
-- classFieldList = classFields . mapAsFieldList

