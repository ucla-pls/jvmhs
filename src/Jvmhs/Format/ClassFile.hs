{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE EmptyCase   #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Jvmhs.Format.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describes conversion from @jvm-binary@'s 'ClassFile' format to
this Class format. Every variable in this module represents an partial
iso-morphism from the ClassFile format and back.

-}
module Jvmhs.Format.ClassFile where

-- base
import           Control.Category
import           Control.Monad
import           Data.Coerce
import           Data.Maybe
import           Data.Either
import           Data.Traversable
import           Prelude                 hiding ( (.)
                                                , id
                                                )

-- lens
import           Control.Lens

-- text 
import qualified Data.Text                     as Text

-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.ConstantValue
                                               as B
import qualified Language.JVM.Attribute.Annotations
                                               as B
import qualified Language.JVM.Attribute.Signature
                                               as B
import qualified Language.JVM.Attribute.Exceptions
                                               as B
import qualified Language.JVM.Attribute.Code   as B
import qualified Language.JVM.Attribute.InnerClasses
                                               as B
import qualified Language.JVM.Attribute.EnclosingMethod
                                               as B
import qualified Language.JVM.Attribute.BootstrapMethods
                                               as B

import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Annotation
import           Jvmhs.Data.Type
import           Jvmhs.Data.Identifier
import           Jvmhs.Data.BootstrapMethod

import           Jvmhs.Format.Internal


type FormatError = String
type Formatter = PartIso [FormatError]

textSerialize :: B.TextSerializable a => Formatter Text.Text a
textSerialize = PartIso (validateEither . B.deserialize) (pure . B.serialize)

-- | Convert a ConstantValue into a JValue
constantValueFormat :: Formatter [B.ConstantValue B.High] (Maybe JValue)
constantValueFormat = coerceFormat . singletonList


classFormat :: Formatter (B.ClassFile B.High) Class
classFormat = PartIso
  (\B.ClassFile {..} -> do
    let _classAccessFlags = coerce cAccessFlags'
    let _className'       = cThisClass
    let _classVersion     = Just (cMajorVersion, cMinorVersion)

    _classFields  <- there (coerceFormat . isomap fieldFormat) cFields'
    _classMethods <- there (coerceFormat . isomap methodFormat) cMethods'

    ((_classSuper', _classInterfaces, _classTypeParameters), (_classBootstrapMethods, _classEnclosingMethod, _classInnerClasses), _classAnnotations) <-
      there classAttributeFormat ((cSuperClass, cInterfaces), cAttributes)

    let _classSuper = if _className' == "java/lang/Object"
          then Nothing
          else Just _classSuper'

    pure Class { .. }
  )
  (\Class {..} -> do
    let cMagicNumber                   = 0xCAFEBABE
    let cConstantPool                  = ()
    let cAccessFlags'                  = B.BitSet _classAccessFlags
    let cThisClass                     = _className'
    let (cMajorVersion, cMinorVersion) = fromMaybe (52, 0) _classVersion

    cFields'  <- back (coerceFormat . isomap fieldFormat) _classFields
    cMethods' <- back (coerceFormat . isomap methodFormat) _classMethods

    ((cSuperClass, cInterfaces), cAttributes) <- back
      classAttributeFormat
      ( ( fromMaybe (withNoAnnotation (classTypeFromName "java/lang/Object"))
                    _classSuper
        , _classInterfaces
        , _classTypeParameters
        )
      , (_classBootstrapMethods, _classEnclosingMethod, _classInnerClasses)
      , _classAnnotations
      )

    pure B.ClassFile { .. }
  )

classAttributeFormat
  :: Formatter
       ((ClassName, B.SizedList16 ClassName), B.ClassAttributes B.High)
       ( (Annotated ClassType, [Annotated ClassType], [Annotated TypeParameter])
       , ([BootstrapMethod], Maybe (ClassName, Maybe MethodId), [InnerClass])
       , Annotations
       )
classAttributeFormat =
  triple classSignatureFormat classExtras runtimeAnnotationsFormat
    . fromIso
        (\((sc, itf), (s, bs, em, ic, tann, an)) ->
          ((((sc, itf), s), tann), (bs, em, ic), an)
        )
        (\((((sc, itf), s), tann), (bs, em, ic), an) ->
          ((sc, itf), (s, bs, em, ic, tann, an))
        )
    . inSecond unwrapClassAttributes
 where
  unwrapClassAttributes = fromIso
    (\B.ClassAttributes {..} ->
      ( caSignature
      , caBootstrapMethods
      , caEnclosingMethod
      , caInnerClasses
      , (caVisibleTypeAnnotations, caInvisibleTypeAnnotations)
      , (caVisibleAnnotations    , caInvisibleAnnotations)
      )
    )
    (\(caSignature, caBootstrapMethods, caEnclosingMethod, caInnerClasses, (caVisibleTypeAnnotations, caInvisibleTypeAnnotations), (caVisibleAnnotations, caInvisibleAnnotations)) ->
      let caOthers = [] in B.ClassAttributes { .. }
    )

  classSignatureFormat
    :: Formatter
         ( ((ClassName, B.SizedList16 ClassName), [B.Signature B.High])
         , ( [B.RuntimeVisibleTypeAnnotations B.ClassTypeAnnotation B.High]
           , [B.RuntimeInvisibleTypeAnnotations B.ClassTypeAnnotation B.High]
           )
         )
         (Annotated ClassType, [Annotated ClassType], [Annotated TypeParameter])
  classSignatureFormat =
    annotateClass
      . (   withDefaultF
        .   (   fromIso
                (\(cn, lst) ->
                  (classTypeFromName cn, map classTypeFromName $ coerce lst, [])
                )
                (\(ct, intf, _) ->
                  ( classNameFromType ct
                  , B.SizedList $ map classNameFromType intf
                  )
                )

            *** ( isomap
                    ( classSignatureFormat'
                    . textSerialize @B.ClassSignature
                    . coerceFormat
                    )
                . singletonList
                )
            )
        *** typeAnnotationsFormat
        )

  classSignatureFormat'
    :: Formatter B.ClassSignature (ClassType, [ClassType], [TypeParameter])
  classSignatureFormat' =
    triple classTypeFormat (isomap classTypeFormat) (isomap typeParameterFormat)
      . mkBClassSignature

  mkBClassSignature = fromIso
    (\B.ClassSignature {..} ->
      (csSuperclassSignature, csInterfaceSignatures, csTypeParameters)
    )
    (\(csSuperclassSignature, csInterfaceSignatures, csTypeParameters) ->
      B.ClassSignature { .. }
    )

  classExtras
    :: Formatter
         ( [B.BootstrapMethods B.High]
         , [B.EnclosingMethod B.High]
         , [B.InnerClasses B.High]
         )
         ([BootstrapMethod], Maybe (ClassName, Maybe MethodId), [InnerClass])
  classExtras = triple
    (compressList bootstrapMethodsFormat)
    ( isomap
        (fromIso (\(B.EnclosingMethod cn mid) -> (cn, mid))
                 (\(cn, mid) -> B.EnclosingMethod cn mid)
        )
    . singletonList
    )
    (compressList innerClassesFormat)

  -- TODO: More work needed
  annotateClass
    :: Formatter
         ( (ClassType, [ClassType], [TypeParameter])
         , [(B.ClassTypeAnnotation B.High, (TypePath, Annotation))]
         )
         (Annotated ClassType, [Annotated ClassType], [Annotated TypeParameter])
  annotateClass = fromIso
    (\((ct, inn, tp), _) ->
      (withNoAnnotation ct, map withNoAnnotation inn, map withNoAnnotation tp)
    )
    (\(ct, inn, tp) ->
      ( ( view annotatedContent ct
        , map (view annotatedContent) inn
        , map (view annotatedContent) tp
        )
      , []
      )
    )

bootstrapMethodsFormat
  :: Formatter (B.BootstrapMethods B.High) [BootstrapMethod]
bootstrapMethodsFormat = coerceFormat
{-# INLINE bootstrapMethodsFormat #-}

innerClassesFormat :: Formatter (B.InnerClasses B.High) [InnerClass]
innerClassesFormat =
  isomap
      (fromIso
        (\B.InnerClass {..} -> InnerClass
          { _innerClass       = icClassName
          , _innerOuterClass  = icOuterClassName
          , _innerClassName   = icInnerName
          , _innerAccessFlags = B.toSet icInnerAccessFlags
          }
        )
        (\InnerClass {..} -> B.InnerClass
          { icClassName        = _innerClass
          , icOuterClassName   = _innerOuterClass
          , icInnerName        = _innerClassName
          , icInnerAccessFlags = B.BitSet _innerAccessFlags
          }
        )
      )
    . coerceFormat

methodFormat :: Formatter (B.Method B.High) Method
methodFormat = PartIso methodThere methodBack where
  methodThere m = do
    let _methodName        = B.mName m
    let _methodAccessFlags = B.mAccessFlags m

    ((_methodTypeParameters, _methodParameters, _methodReturnType, _methodExceptions), _methodCode, _methodAnnotations) <-
      there methodAttributesFormat (B.mDescriptor m, B.mAttributes m)

    pure Method { .. }

  methodBack m = do
    let mName         = m ^. methodName
    let mAccessFlags' = B.BitSet (m ^. methodAccessFlags)

    (mDescriptor, mAttributes) <- back
      methodAttributesFormat
      ( ( m ^. methodTypeParameters
        , m ^. methodParameters
        , m ^. methodReturnType
        , m ^. methodExceptions
        )
      , m ^. methodCode
      , m ^. methodAnnotations
      )
    pure B.Method { .. }

methodAttributesFormat
  :: Formatter
       (B.MethodDescriptor, B.MethodAttributes B.High)
       ( ( [Annotated TypeParameter]
         , [Annotated Type]
         , Annotated ReturnType
         , [Annotated ThrowsType]
         )
       , Maybe Code
       , Annotations
       )
methodAttributesFormat =
  triple
      ( methodSignatureFormat
      . triple
          (   (inSecond $ compressList coerceFormat)
          *** ( isomap (textSerialize @B.MethodSignature . coerceFormat)
              . singletonList
              )
          )
          parameterAnnotationsFormat
          typeAnnotationsFormat
      )
      (isomap codeFormat . singletonList)
      runtimeAnnotationsFormat
    . PartIso anThere anBack where

  anThere (desc, B.MethodAttributes {..}) = do
    return
      ( ( ((desc, maExceptions)         , maSignatures)
        , (maVisibleParameterAnnotations, maInvisibleParameterAnnotations)
        , (maVisibleTypeAnnotations     , maInvisibleTypeAnnotations)
        )
      , maCode
      , (maVisibleAnnotations, maInvisibleAnnotations)
      )

  anBack ((((desc, maExceptions), maSignatures), (maVisibleParameterAnnotations, maInvisibleParameterAnnotations), (maVisibleTypeAnnotations, maInvisibleTypeAnnotations)), maCode, (maVisibleAnnotations, maInvisibleAnnotations))
    = do
      let maAnnotationDefault = []
      let maOthers            = []
      return (desc, B.MethodAttributes { .. })

methodSignatureFormat
  :: Formatter
       ( ((B.MethodDescriptor, [ClassName]), Maybe B.MethodSignature)
       , [Annotations]
       , [(B.MethodTypeAnnotation B.High, (TypePath, Annotation))]
       )
       ( [Annotated TypeParameter]
       , [Annotated Type]
       , Annotated ReturnType
       , [Annotated ThrowsType]
       )
methodSignatureFormat =
  PartIso msThere msBack . fromLens _1 _1 moreSignatureFormat
 where

  msThere ((tp, prams, rt, thrws), _, _) = do
    return
      ( map withNoAnnotation tp
      , map withNoAnnotation prams
      , withNoAnnotation rt
      , map withNoAnnotation thrws
      )

  msBack (tps, ts, rt, thrs) = do
    return
      ( ( map (view annotatedContent) tps
        , map (view annotatedContent) ts
        , view annotatedContent rt
        , map (view annotatedContent) thrs
        )
      , []
      , []
      )

  moreSignatureFormat
    :: Formatter
         ((B.MethodDescriptor, [ClassName]), Maybe B.MethodSignature)
         ([TypeParameter], [Type], ReturnType, [ThrowsType])
  moreSignatureFormat =
    withDefaultF . (unwrapMethodDescriptor *** isomap unwrapMethodSignature)

  unwrapMethodSignature
    :: Formatter
         B.MethodSignature
         ([TypeParameter], [Type], ReturnType, [ThrowsType])
  unwrapMethodSignature = PartIso mThere mBack   where
    mThere B.MethodSignature {..} = do
      tp    <- mapM (there typeParameterFormat) msTypeParameters
      tps   <- mapM (there typeFormat) msArguments
      rt    <- there returnTypeFormat msResults
      thrws <- mapM (there throwsTypeFormat) msThrows

      return (tp, tps, rt, thrws)

    mBack (tp, tps, rt, thrws) = do
      msTypeParameters <- mapM (back typeParameterFormat) tp
      msArguments      <- mapM (back typeFormat) tps
      msResults        <- back returnTypeFormat rt
      msThrows         <- mapM (back throwsTypeFormat) thrws

      pure B.MethodSignature { .. }


  unwrapMethodDescriptor
    :: Formatter
         (B.MethodDescriptor, [ClassName])
         ([TypeParameter], [Type], ReturnType, [ThrowsType])
  unwrapMethodDescriptor = fromIso
    (\(md, thrws) ->
      ( []
      , map fromJType (methodDescriptorArguments md)
      , ReturnType
        . fmap fromJType
        . B.asMaybeJType
        . methodDescriptorReturnType
        $ md
      , fmap (ThrowsClass . classTypeFromName) thrws
      )
    )
    (\(_, prm, ReturnType rt, thrws) ->
      ( B.MethodDescriptor
        (map (fromRight "Ljava/lang/Object;" . toJType) prm)
        ( B.ReturnDescriptor
        . fmap (fromRight "Ljava/lang/Object;" . toJType)
        $ rt
        )
      , fmap (fromRight "java/lang/Object" . classNameFromThrowsType) thrws
      )
    )


codeFormat :: Formatter (B.Code B.High) Code
codeFormat = PartIso
  (\c -> do
    let _codeMaxStack  = B.codeMaxStack c
    let _codeMaxLocals = B.codeMaxLocals c
    _codeExceptionTable <- mapM (there exceptionHandlerFormat)
                                (coerce (B.codeExceptionTable c))
    let _codeByteCode = B.unByteCode (B.codeByteCode c)
    _codeStackMap <- there codeAttributesFormat (B.codeAttributes c)
    pure Code { .. }
  )
  (\Code {..} -> do
    codeExceptionTable' <- fmap coerce
      $ mapM (back exceptionHandlerFormat) _codeExceptionTable
    codeAttributes' <- back codeAttributesFormat Nothing
    pure B.Code { codeMaxStack       = _codeMaxStack
                , codeMaxLocals      = _codeMaxLocals
                , codeByteCode       = B.ByteCode 0 _codeByteCode
                , codeExceptionTable = codeExceptionTable'
                , codeAttributes     = codeAttributes'
                }
  )

codeAttributesFormat
  :: Formatter (B.CodeAttributes B.High) (Maybe (B.StackMapTable B.High))
codeAttributesFormat = PartIso
  (\B.CodeAttributes {..} -> there singletonList caStackMapTable)
  (\a -> do
    caStackMapTable <- back singletonList a
    let caLineNumberTable          = []
    let caVisibleTypeAnnotations   = []
    let caInvisibleTypeAnnotations = []
    let caOthers                   = []
    pure B.CodeAttributes { .. }
  )

exceptionHandlerFormat :: Formatter (B.ExceptionTable B.High) ExceptionHandler
exceptionHandlerFormat = PartIso
  (\B.ExceptionTable {..} ->
    let _ehStart     = start
        _ehEnd       = end
        _ehHandler   = handler
        _ehCatchType = catchType
    in  pure ExceptionHandler { .. }
  )
  (\ExceptionHandler {..} ->
    let start     = _ehStart
        end       = _ehEnd
        handler   = _ehHandler
        catchType = _ehCatchType
    in  pure B.ExceptionTable { .. }
  )

-- | Convert a `B.Field` to a `Field` 
fieldFormat :: Formatter (B.Field B.High) Field
fieldFormat = PartIso { there = fieldThere, back = fieldBack } where
  fieldThere f = do
    let _fieldName        = B.fName f
    let desc              = B.fDescriptor f
    let _fieldAccessFlags = B.fAccessFlags f

    ((_fieldValue, _fieldAnnotations), _fieldType) <- there
      fieldAttributesFormat
      (desc, B.fAttributes f)

    pure Field { .. }

  fieldBack f = do
    let fName         = f ^. fieldName
    let fAccessFlags' = B.BitSet (f ^. fieldAccessFlags)

    (fDescriptor, fAttributes) <- back
      fieldAttributesFormat
      ((f ^. fieldValue, f ^. fieldAnnotations), f ^. fieldType)

    pure B.Field { .. }

fieldTypeFormat
  :: Formatter
       ([(TypePath, Annotation)], (B.FieldDescriptor, [B.Signature B.High]))
       (Annotated Type)
fieldTypeFormat = annotateType . inSecond
  ( withDefaultF
  . (   (typeFromJTypeFormat (const "Ljava/lang/Object;") . coerceFormat)
    *** fieldTypeFromSignature
    )
  )
 where
  fieldTypeFromSignature :: Formatter [B.Signature B.High] (Maybe Type)
  fieldTypeFromSignature =
    (flipDirection $ fromPrism _ReferenceType)
      . isomap (referenceTypeFromSignature . signatureFormat)
      . singletonList

  signatureFormat :: Formatter (B.Signature B.High) B.ReferenceType
  signatureFormat =
    coerceFormat . textSerialize @B.FieldSignature . coerceFormat

  annotateType :: Formatter ([(TypePath, Annotation)], Type) (Annotated Type)
  annotateType = PartIso anThere anBack   where

    anThere
      :: ([(TypePath, Annotation)], Type)
      -> Validation [FormatError] (Annotated Type)
    anThere (p, t) = validateEither $ setTypeAnnotations p (withNoAnnotation t)

    anBack
      :: Annotated Type
      -> Validation [FormatError] ([(TypePath, Annotation)], Type)
    anBack a = Success (getTypeAnnotations a, view annotatedContent a)


fieldAttributesFormat
  :: Formatter
       (B.FieldDescriptor, B.FieldAttributes B.High)
       ((Maybe JValue, Annotations), Annotated Type)
fieldAttributesFormat = inSecond fieldTypeFormat . PartIso attrThere attrBack where
  attrThere (fdesc, fattr) = do
    mjv <- there constantValueFormat (B.faConstantValues fattr)
    ann <- there
      runtimeAnnotationsFormat
      (B.faVisibleAnnotations fattr, B.faInvisibleAnnotations fattr)

    tpann <- there
      (typeAnnotationsFormat @B.FieldTypeAnnotation)
      (B.faVisibleTypeAnnotations fattr, B.faInvisibleTypeAnnotations fattr)

    pure ((mjv, ann), (map (view _2) tpann, (fdesc, B.faSignatures fattr)))

  attrBack ((mjv, ann), (tpann, (fdesc, faSignatures))) = do
    faConstantValues <- back constantValueFormat mjv

    (faVisibleAnnotations, faInvisibleAnnotations) <- back
      runtimeAnnotationsFormat
      ann

    (faVisibleTypeAnnotations, faInvisibleTypeAnnotations) <- back
      (typeAnnotationsFormat @B.FieldTypeAnnotation)
      (map (B.FieldTypeAnnotation, ) tpann)

    let faOthers = []

    pure (fdesc, B.FieldAttributes { .. })



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

parameterAnnotationsFormat
  :: Formatter
       ( [B.RuntimeVisibleParameterAnnotations B.High]
       , [B.RuntimeInvisibleParameterAnnotations B.High]
       )
       [Annotations]
parameterAnnotationsFormat =
  isomap (flipDirection (partitionList (view annotationIsRuntimeVisible)))
    . zipList
    . (   runtimeVisibleParameterAnnotationsFormat
      *** runtimeInvisibleParameterAnnotationsFormat
      )
 where
  runtimeVisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeVisibleParameterAnnotations B.High] [Annotations]
  runtimeVisibleParameterAnnotationsFormat =
    isomap (annotationsFormat True) . compressList coerceFormat

  runtimeInvisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeInvisibleParameterAnnotations B.High] [Annotations]
  runtimeInvisibleParameterAnnotationsFormat =
    isomap (annotationsFormat False) . compressList coerceFormat

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



--   -- | Create annotations
-- mkAnnotations :: PartIso e (AnnotationMap, AnnotationMap) Annotations
-- mkAnnotations = fromIso
--   (uncurry Annotations)
--   ((,) <$> view visibleAnnotations <*> view invisibleAnnotations)

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



-- | A SizedList is just a list
sizedList32Format :: Formatter (B.SizedList16 a) [a]
sizedList32Format = coerceFormat

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile

toClassFile :: Class -> B.ClassFile B.High
toClassFile = undefined

fromClassFile :: B.ClassFile B.High -> Class
fromClassFile = undefined


-- * Signatures

typeParameterFormat :: Formatter B.TypeParameter TypeParameter
typeParameterFormat = PartIso
  (\B.TypeParameter {..} -> do
    _typeInterfaceBound <- mapM (there referenceTypeFromSignature)
                                tpInterfaceBound
    _typeClassBound <- mapM (there referenceTypeFromSignature) tpClassBound
    let _typeIdentifier = tpIdentifier
    pure TypeParameter { .. }
  )
  (\TypeParameter {..} -> do
    tpInterfaceBound <- mapM (back referenceTypeFromSignature)
                             _typeInterfaceBound
    tpClassBound <- mapM (back referenceTypeFromSignature) _typeClassBound
    let tpIdentifier = _typeIdentifier
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

typeFromJTypeFormat :: (TypeVariable -> B.JRefType) -> Formatter B.JType Type
typeFromJTypeFormat fn = fromIso fromJType (either (B.JTRef . fn) id . toJType)

typeFromJRefTypeFormat :: (TypeVariable -> B.JRefType) -> Formatter B.JType Type
typeFromJRefTypeFormat fn =
  fromIso fromJType (either (B.JTRef . fn) id . toJType)

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
typeVariableFromSignature = coerceFormat

-- | Create a classType from a signature. The semantics of this construct
-- changes meaning. The ClassType defined in this library have slots for
-- all innerclasses. Where the original compresses the last innerclasses
-- into one class.
classTypeFormat :: Formatter B.ClassType ClassType
classTypeFormat = PartIso
  (\(B.ClassType n ict ta) -> do
    ta'  <- mapM (there typeArgumentFromSignature) ta
    ict' <- mapM thereInnerClass ict
    let n' =
          classTypeFromName n & (classTypeArguments .~ map withNoAnnotation ta')
    return $ case ict' of
      Just i  -> extendClassType i n'
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
      Nothing             -> pure (n, Nothing, ta')
      Just (n'', t'', []) -> pure (n <> "$" <> n'', t'', ta')
      Just (n'', t'', ta'') ->
        pure (n, Just $ B.InnerClassType n'' t'' ta'', ta')

  thereInnerClass (B.InnerClassType n ict ta) = do
    ta'  <- mapM (there typeArgumentFromSignature) ta
    ict' <- mapM thereInnerClass ict
    let n' =
          innerClassTypeFromName n
            & (classTypeArguments .~ map withNoAnnotation ta')
    return $ case ict' of
      Just i  -> extendClassType i n'
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
--
-- fromClassFile :: B.ClassFile B.High -> Class
-- fromClassFile = do
--   _className <- B.cThisClass
--
--   classSignature <-
--     fmap (\(Signature a) ->
--             fromRight (error $ "could not read " ++ show a) $ classSignatureFromText a
--          ) . B.cSignature
--
--   let _classTypeParameters = concatMap csTypeParameters classSignature
--
--   _classSuper <- \cls ->
--     case fmap csSuperclassSignature classSignature of
--       _ | B.cThisClass cls == "java/lang/Object" ->
--           Nothing
--       Just sp
--         -> Just sp
--       Nothing
--         ->
--           Just . classTypeFromName $ B.cSuperClass cls
--
--   _classAccessFlags <-
--     B.cAccessFlags
--
--   _classInterfaces <- \cls ->
--     maybe [ classTypeFromName i | i <- toListOf (folding B.cInterfaces) cls ]
--       csInterfaceSignatures classSignature
--
--   _classFields <-
--     toListOf (folded.from _Binary) . B.cFields
--
--   _classMethods <-
--     toListOf (folded.from _Binary) . B.cMethods
--
--   _classBootstrapMethods <-
--     map fromBinaryBootstrapMethod . B.cBootstrapMethods
--
--   _classEnclosingMethod <-
--     fmap (\e ->
--             ( B.enclosingClassName e
--             , B.enclosingMethodName e
--             )
--          ) . B.cEnclosingMethod
--
--   _classInnerClasses <-
--     toListOf (folded.from _Binary) . B.cInnerClasses
--
--   _classAnnotations <-
--     (readAnnotations <$> B.caVisibleAnnotations <*> B.caInvisibleAnnotations)
--     . B.cAttributes
--
--   _classVersion  <-
--     Just <$> ((,) <$> B.cMajorVersion <*> B.cMinorVersion)
--
--   pure $ Class (Named _className (ClassContent {..}))
--
-- readAnnotations
--     ::         [B.RuntimeVisibleAnnotations B.High]
--     ->         [B.RuntimeInvisibleAnnotations B.High]
--     ->         Annotations
-- readAnnotations vis invis = Annotations
--   (concatMap (toList . B.asListOfRuntimeVisibleAnnotations) vis)
--   (concatMap (toList . B.asListOfRuntimeInvisibleAnnotations) invis)
--
-- toClassFile :: Class -> B.ClassFile B.High
-- toClassFile =
--
--   B.ClassFile 0xCAFEBABE
--     <$> maybe (0 :: Word16) snd . view classVersion
--     <*> maybe (52 :: Word16) fst . view classVersion
--     <*> pure ()
--     <*> B.BitSet . view classAccessFlags
--
--     <*> view className
--     <*> maybe "java/lang/Object" (view classTypeName)
--       . view classSuper
--
--     <*> B.SizedList . toListOf ( classInterfaces.folded.classTypeName)
--     <*> B.SizedList . toListOf ( classFields.folded._Binary )
--     <*> B.SizedList . toListOf ( classMethods.folded._Binary )
--     <*> ( do
--            caBootstrapMethods <-
--              compress (B.BootstrapMethods . B.SizedList)
--              . map toBinaryBootstrapMethod
--              . view classBootstrapMethods
--
--            caSignature <-
--              maybeToList
--              . fmap (Signature . classSignatureToText)
--              . (\cls ->
--                  case cls^.classSuper of
--                    Just x ->
--                      Just $ ClassSignature (cls^.classTypeParameters) x (cls^.classInterfaces)
--                    Nothing ->
--                      Nothing
--                )
--
--            caEnclosingMethod <-
--              maybeToList
--              . fmap (uncurry B.EnclosingMethod)
--              . view classEnclosingMethod
--
--            caInnerClasses <-
--              compress B.InnerClasses
--              . map (view _Binary)
--              . view classInnerClasses
--
--            caVisibleAnnotations <-
--              compress (B.RuntimeVisibleAnnotations . B.SizedList)
--              . view (classAnnotations . visibleAnnotations)
--
--            caInvisibleAnnotations <-
--              compress (B.RuntimeInvisibleAnnotations . B.SizedList)
--              . view (classAnnotations . invisibleAnnotations)
--
--            caVisibleTypeAnnotations <- pure []
--            caInvisibleTypeAnnotations <- pure []
--            caOthers <- pure []
--
--            pure $ B.ClassAttributes {..}
--        )
--
--
