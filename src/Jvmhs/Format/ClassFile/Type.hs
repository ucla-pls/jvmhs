{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-|
Module      : Jvmhs.Format.ClassFile.Type
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

-}
module Jvmhs.Format.ClassFile.Type where

-- base
import           Control.Category
import           Control.Monad
import           Data.Bifunctor
import           Data.Maybe
import           Data.Traversable
import           Prelude                 hiding ( id
                                                , (.)
                                                )

-- lens
import           Control.Lens

-- text
import qualified Data.Text                     as Text


-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B
import qualified Language.JVM.Attribute.ConstantValue
                                               as B
import qualified Language.JVM.Attribute.Signature
                                               as B

import           Jvmhs.Data.Type
import           Jvmhs.Data.Identifier
import           Jvmhs.Format.Internal
import           Jvmhs.Format.ClassFile.Shared


typeParameterFormat :: Formatter B.TypeParameter TypeParameter
typeParameterFormat = PartIso
  (\B.TypeParameter {..} -> do
    _typeParameterInterfaceBound <- mapM
      (fmap withNoAnnotation . there throwsTypeFromReferenceType)
      tpInterfaceBound
    _typeParameterClassBound <- mapM
      (fmap withNoAnnotation . there throwsTypeFromReferenceType)
      tpClassBound
    let _typeParameterName = TypeVariableName tpIdentifier
    pure TypeParameter { .. }
  )
  (\TypeParameter {..} -> do
    tpInterfaceBound <- mapM
      (back throwsTypeFromReferenceType . view annotatedContent)
      _typeParameterInterfaceBound
    tpClassBound <- mapM
      (back throwsTypeFromReferenceType . view annotatedContent)
      _typeParameterClassBound
    let tpIdentifier = _typeParameterName ^. unTypeVariableName
    pure B.TypeParameter { .. }
  )

returnTypeFormat :: Formatter (Maybe B.TypeSignature) ReturnType
returnTypeFormat = PartIso (fmap ReturnType . mapM (there typeFormat))
                           (mapM (back typeFormat) . view returnType)

throwsTypeFormat :: Formatter (B.ThrowsSignature) ThrowsType
throwsTypeFormat = PartIso
  (\case
    B.ThrowsClass cn -> ThrowsClass <$> there classTypeFormat cn
    B.ThrowsTypeVariable tv ->
      ThrowsTypeVariable <$> there typeVariableFromSignature tv
  )
  (\case
    ThrowsClass cn -> B.ThrowsClass <$> back classTypeFormat cn
    ThrowsTypeVariable tv ->
      B.ThrowsTypeVariable <$> back typeVariableFromSignature tv
  )

  -- mkBMethodAttributes = fromIso
  --   (\((maSignatures, maExceptions, parm, type_), maCode, anno) ->
  --     let (maInvisibleParameterAnnotations, maVisibleParameterAnnotations) =
  --             parm
  --         (maInvisibleTypeAnnotations, maVisibleTypeAnnotations) = type_
  --         (maInvisibleAnnotations    , maVisibleAnnotations    ) = anno
  --     in  B.MethodAttributes { .. }
  --   )
  --   (\B.MethodAttributes {..} ->
  --     ( ( maSignatures
  --       , maExceptions
  --       , (maInvisibleParameterAnnotations, maVisibleParameterAnnotations)
  --       , (maInvisibleTypeAnnotations     , maVisibleTypeAnnotations)
  --       )
  --     , maCode
  --     , (maInvisibleAnnotations, maVisibleAnnotations)
  --     )
  --   )

typeFromJTypeFormat :: Formatter B.JType Type
typeFromJTypeFormat = fromIso fromJType toBoundJType

-- typeFromJRefTypeFormat :: (TypeVariable -> B.JRefType) -> Formatter B.JType Type
-- typeFromJRefTypeFormat fn =
--   fromIso fromJType (either (B.JTRef . fn) id . toJType)

-- | This exist because many places it is actually impossible to have
-- refence types in the possition of the types. This will just parse 
-- it as a classType or throw an error
classTypeFromReferenceType :: Formatter B.ReferenceType ClassType
classTypeFromReferenceType =
  fromPrism'
      (\a -> Failure
        [ "Expected this type to be classes but got "
          ++ show a
          ++ ", please report a bug."
        ]
      )
      (_RefClassType)
    . referenceTypeFromSignature

-- | This exist because many places it is actually impossible to have
-- arrays types in the possition of the types. This will just parse 
-- it as a ThrowsType or throw an error
throwsTypeFromReferenceType :: Formatter B.ReferenceType ThrowsType
throwsTypeFromReferenceType =
  PartIso
      (\case
        RefClassType    t  -> Success $ ThrowsClass t
        RefTypeVariable tv -> Success $ ThrowsTypeVariable tv
        a                  -> Failure
          [ "Expected this type to be a throws type but got "
            ++ show a
            ++ ", please report a bug."
          ]
      )
      (\case
        ThrowsClass        t  -> Success $ RefClassType t
        ThrowsTypeVariable tv -> Success $ RefTypeVariable tv
      )
    . referenceTypeFromSignature

referenceTypeFromSignature :: Formatter B.ReferenceType ReferenceType
referenceTypeFromSignature = PartIso
  (\case
    B.RefClassType ct -> RefClassType <$> there classTypeFormat ct
    B.RefTypeVariable tv ->
      RefTypeVariable <$> there typeVariableFromSignature tv
    B.RefArrayType atp ->
      RefArrayType . ArrayType . withNoAnnotation <$> there typeFormat atp
  )
  (\case
    RefClassType ct -> B.RefClassType <$> back classTypeFormat ct
    RefTypeVariable tv ->
      B.RefTypeVariable <$> back typeVariableFromSignature tv
    RefArrayType (ArrayType (Annotated atp _)) ->
      B.RefArrayType <$> back typeFormat atp
  )

typeFormat :: Formatter B.TypeSignature Type
typeFormat = PartIso
  (\case
    B.ReferenceType r -> ReferenceType <$> there referenceTypeFromSignature r
    B.BaseType      b -> pure $ BaseType b
  )
  (\case
    ReferenceType r -> B.ReferenceType <$> back referenceTypeFromSignature r
    BaseType      b -> pure $ B.BaseType b
  )

typeVariableFromSignature :: Formatter B.TypeVariable TypeVariable
typeVariableFromSignature =
  fromIso unboundTypeVariable (view typeVariableName) . coerceFormat

-- | Create a classType from a signature. The semantics of this construct
-- changes meaning. The ClassType defined in this library have slots for
-- all innerclasses. Where the original compresses the last innerclasses
-- into one class.
classTypeFormat :: Formatter B.ClassType ClassType
classTypeFormat = PartIso
  (\(B.ClassType n ict ta) -> do
    ta'  <- mapM (there typeArgumentFromSignature) ta
    ict' <- mapM thereInnerClass ict
    let n' = classTypeFromNameAndTypeArgs n (map withNoAnnotation ta')
    return $ case ict' of
      Just i  -> extendClassType (withNoAnnotation i) n'
      Nothing -> n'
  )
  (go >=> \(n, t, ta) -> do
    nm <- validateEither (B.textCls n)
    pure $ B.ClassType nm t ta
  )
 where
  go
    :: ClassType
    -> Validation
         [FormatError]
         (Text.Text, Maybe B.InnerClassType, [Maybe B.TypeArgument])
  go (ClassType n t ta) = do
    ta' <- mapM (back typeArgumentFromSignature . view annotatedContent) ta
    traverse (go . view annotatedContent) t >>= \case
      Nothing               -> pure (n, Nothing, ta')
      Just (n'', t'', ta'') -> case ta' of
        [] -> pure (n <> "$" <> n'', t'', ta'')
        ts -> pure (n, Just $ B.InnerClassType n'' t'' ta'', ts)

  thereInnerClass (B.InnerClassType n ict ta) = do
    ta'  <- mapM (there typeArgumentFromSignature) ta
    ict' <- mapM thereInnerClass ict
    let
      n' =
        insertTypeArgument (map withNoAnnotation ta') (innerClassTypeFromName n)
    return $ case ict' of
      Just i  -> extendClassType (withNoAnnotation i) n'
      Nothing -> n'

typeArgumentFromSignature :: Formatter (Maybe B.TypeArgument) TypeArgument
typeArgumentFromSignature = PartIso
  (\case
    Just (B.TypeArgument mw rt) -> do
      rt' <- there referenceTypeFromSignature rt
      pure $ case mw of
        Nothing          -> TypeArg rt'
        Just B.WildPlus  -> ExtendedTypeArg (withNoAnnotation rt')
        Just B.WildMinus -> ImplementedTypeArg (withNoAnnotation rt')
    Nothing -> pure AnyTypeArg
  )
  (\case
    AnyTypeArg -> pure $ Nothing
    TypeArg rt -> do
      Just . B.TypeArgument Nothing <$> back referenceTypeFromSignature rt
    ExtendedTypeArg rt -> do
      Just . B.TypeArgument (Just B.WildPlus) <$> back
        referenceTypeFromSignature
        (view annotatedContent rt)

    ImplementedTypeArg rt -> do
      Just . B.TypeArgument (Just B.WildMinus) <$> back
        referenceTypeFromSignature
        (view annotatedContent rt)
  )

-- | Convert a ConstantValue into a JValue
constantValueFormat :: Formatter [B.ConstantValue B.High] (Maybe JValue)
constantValueFormat = coerceFormat . singletonList

-- * Annotations

mkBAnnotation
  :: Formatter (FieldDescriptor, [B.ValuePair B.High]) (B.Annotation B.High)
mkBAnnotation =
  fromIso (uncurry B.Annotation)
          ((,) <$> B.annotationType <*> B.annotationValuePairs)
    . (inSecond coerceFormat)


annotationValueFormat :: Formatter (B.ElementValue B.High) AnnotationValue
annotationValueFormat = PartIso { there = valueThere, back = valueBack } where
  valueThere = \case
    B.EByte           i -> pure $ AByte i
    B.EShort          i -> pure $ AShort i
    B.EChar           i -> pure $ AChar i
    B.EFloat          i -> pure $ AFloat i
    B.EInt            i -> pure $ AInt i
    B.EDouble         i -> pure $ ADouble i
    B.ELong           i -> pure $ ALong i
    B.EBoolean        i -> pure $ ABoolean i
    B.EString         i -> pure $ AString i
    B.EEnum           i -> pure $ AEnum i
    B.EClass          c -> pure $ AClass c
    B.EAnnotationType a -> do
      (n, c) <- there (annotationMapFormat . (flipDirection mkBAnnotation)) a
      pure $ AAnnotation (n, c)
    B.EArrayType (B.SizedList as) ->
      AArray <$> there (isomap annotationValueFormat) as

  valueBack = \case
    AByte       i      -> pure $ B.EByte i
    AShort      i      -> pure $ B.EShort i
    AChar       i      -> pure $ B.EChar i
    AFloat      i      -> pure $ B.EFloat i
    AInt        i      -> pure $ B.EInt i
    ADouble     i      -> pure $ B.EDouble i
    ALong       i      -> pure $ B.ELong i
    ABoolean    i      -> pure $ B.EBoolean i
    AString     i      -> pure $ B.EString i
    AEnum       i      -> pure $ B.EEnum i
    AClass      c      -> pure $ B.EClass c
    AAnnotation (t, c) -> B.EAnnotationType
      <$> back (annotationMapFormat . (flipDirection mkBAnnotation)) (t, c)
    AArray a -> do
      ev <- back (isomap annotationValueFormat) a
      pure . B.EArrayType $ B.SizedList ev


annotationMapFormat
  :: Formatter
       (FieldDescriptor, [B.ValuePair B.High])
       (ClassName, AnnotationMap)
annotationMapFormat =
  (   (expectClass . coerceFormat)
  *** (mkHashMap . isomap
        (inSecond annotationValueFormat . flipDirection mkBValuePair)
      )
  )
 where
  mkBValuePair
    :: Formatter (Text.Text, B.ElementValue B.High) (B.ValuePair B.High)
  mkBValuePair = fromIso (uncurry B.ValuePair) ((,) <$> B.name <*> B.value)

  expectClass :: Formatter JType ClassName
  expectClass = fromPrism'
    (\a -> Failure
      [ "Expected all annotation types to be classes but got "
        ++ show a
        ++ ", please report a bug."
      ]
    )
    (_JTRef . _JTClass)

annotationsFormat :: Bool -> Formatter [B.Annotation B.High] Annotations
annotationsFormat visible =
  isomap (mkAnnotation . annotationMapFormat . flipDirection mkBAnnotation)
    . coerceFormat

 where
  mkAnnotation :: Formatter (ClassName, AnnotationMap) Annotation
  mkAnnotation =
    fromIso (\(a, c) -> Annotation a visible c) (\(Annotation a _ c) -> (a, c))

typeAnnotationsFormat
  :: forall a
   . Formatter
    ( [B.RuntimeVisibleTypeAnnotations a B.High]
    , [B.RuntimeInvisibleTypeAnnotations a B.High]
    )
    [(a B.High, (TypePath, Annotation))]
typeAnnotationsFormat =
  flipDirection (partitionList (view (_2 . _2 . annotationIsRuntimeVisible)))
    . (runtimeVisibleAnnotationsFormat *** runtimeInvisibleAnnotationsFormat)
 where
  runtimeVisibleAnnotationsFormat
    :: Formatter
         [B.RuntimeVisibleTypeAnnotations a B.High]
         [(a B.High, (TypePath, Annotation))]

  runtimeVisibleAnnotationsFormat =
    isomap (typeAnnotationFormat True) . compressList coerceFormat

  runtimeInvisibleAnnotationsFormat
    :: Formatter
         [B.RuntimeInvisibleTypeAnnotations a B.High]
         [(a B.High, (TypePath, Annotation))]

  runtimeInvisibleAnnotationsFormat =
    isomap (typeAnnotationFormat False) . compressList coerceFormat


  typeAnnotationFormat
    :: Bool
    -> Formatter
         (B.TypeAnnotation a B.High)
         (a B.High, (TypePath, Annotation))
  typeAnnotationFormat visible =
    inSecond (coerceFormat *** mkAnnotation . annotationMapFormat)
      . flipDirection mkBTypeAnnotation

   where
    mkAnnotation :: Formatter (ClassName, AnnotationMap) Annotation
    mkAnnotation = fromIso (\(a, c) -> Annotation a visible c)
                           (\(Annotation a _ c) -> (a, c))

    mkBTypeAnnotation
      :: Formatter
           (a B.High, (B.TypePath, (FieldDescriptor, [B.ValuePair B.High])))
           (B.TypeAnnotation a B.High)
    mkBTypeAnnotation = fromIso
      (\(a, (tp, (fd, vm))) -> B.TypeAnnotation a tp fd (B.SizedList vm))
      (\(B.TypeAnnotation a tp fd vm) -> (a, (tp, (fd, (B.unSizedList vm)))))

-- | a converation between annotations
runtimeAnnotationsFormat
  :: Formatter
       ( [B.RuntimeVisibleAnnotations B.High]
       , [B.RuntimeInvisibleAnnotations B.High]
       )
       Annotations
runtimeAnnotationsFormat =
  flipDirection (partitionList (view annotationIsRuntimeVisible))
    . (runtimeVisibleAnnotationsFormat *** runtimeInvisibleAnnotationsFormat)
 where
  runtimeVisibleAnnotationsFormat
    :: Formatter [B.RuntimeVisibleAnnotations B.High] Annotations
  runtimeVisibleAnnotationsFormat =
    annotationsFormat True . compressList coerceFormat

  runtimeInvisibleAnnotationsFormat
    :: Formatter [B.RuntimeInvisibleAnnotations B.High] Annotations
  runtimeInvisibleAnnotationsFormat =
    annotationsFormat False . compressList coerceFormat

annotateType
  :: forall a
   . (Show a, HasTypeAnnotations a)
  => (ClassName -> Bool)
  -> Formatter ([(TypePath, Annotation)], a) (Annotated a)
annotateType isStatic = PartIso anThere anBack where
  anThere (p, t) = validateEither
    ( first ((show t ++ ": ") ++)
    $ setTypeAnnotations isStatic p (withNoAnnotation t)
    )
  anBack a = Success (getTypeAnnotations isStatic a, view annotatedContent a)

