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
Module      : Jvmhs.Format.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describes conversion from @jvm-binary@'s 'ClassFile' format to
this Class format.

-}
module Jvmhs.Format.ClassFile where

-- base
import           Control.Category
import           Control.Monad
import           Data.Bifunctor
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Maybe
import           Data.Traversable
import           Prelude                 hiding ( id
                                                , (.)
                                                )

-- containers
import qualified Data.Map.Strict               as Map

-- lens
import           Control.Lens

-- text
import qualified Data.Text                     as Text


-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B
import qualified Language.JVM.Attribute.BootstrapMethods
                                               as B
import qualified Language.JVM.Attribute.Code   as B
import qualified Language.JVM.Attribute.ConstantValue
                                               as B
import qualified Language.JVM.Attribute.EnclosingMethod
                                               as B
import qualified Language.JVM.Attribute.Exceptions
                                               as B
import qualified Language.JVM.Attribute.InnerClasses
                                               as B
import qualified Language.JVM.Attribute.MethodParameters
                                               as B
import qualified Language.JVM.Attribute.Signature
                                               as B

import           Jvmhs.Data.Annotation
import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Identifier
import           Jvmhs.Data.Type

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
            (not . andOf
              (  (_1 . to classTypeIsSimple)
              <> (_2 . folded . to classTypeIsSimple)
              <> (_3 . folded . like False)
              )
            )
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

methodParametersFormat
  :: Formatter (B.MethodParameters B.High) [B.MethodParameter B.High]
methodParametersFormat = coerceFormat
{-# INLINE methodParametersFormat #-}


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

-- | Can convert a list of items into a list of maybe items. Fails if all
-- elements are not either Just b or Nothing.
allOrNothing :: Formatter ([a], Maybe [b]) [(a, Maybe b)]
allOrNothing = PartIso
  (\(as, bs) -> pure $ case bs of
    Nothing  -> map (, Nothing) as
    Just bs' -> zip as (map Just bs')
  )
  (\items -> do
    let (as, catMaybes -> bs) = unzip items
    if null bs
      then Success (as, Nothing)
      else if length bs == length as
        then Success (as, Just bs)
        else Failure ["Both Just's and Nothing's exists in list"]
  )


methodAttributesFormat
  :: Formatter
       (B.MethodDescriptor, B.MethodAttributes B.High)
       ( ( [Annotated TypeParameter]
         , [Parameter]
         , Annotated ReturnType
         , [Annotated ThrowsType]
         )
       , Maybe Code
       , Annotations
       )
methodAttributesFormat =
  triple (annotateMethodTypesFormat . inSecond typeAnnotationsFormat)
         (isomap codeFormat . singletonList)
         runtimeAnnotationsFormat
    . PartIso anThere anBack where

  anThere (desc, B.MethodAttributes {..}) = do
    (tps, parmst, rt, thrws) <- there handleSignature
                                      ((desc, maExceptions), maSignatures)

    params <- there
      handleParameters
      ( (parmst                       , maMethodParameters)
      , (maVisibleParameterAnnotations, maInvisibleParameterAnnotations)
      )

    return
      ( ( (tps, params, rt, thrws)
        , (maVisibleTypeAnnotations, maInvisibleTypeAnnotations)
        )
      , maCode
      , (maVisibleAnnotations, maInvisibleAnnotations)
      )

  anBack (((tps, params, rt, thrws), (maVisibleTypeAnnotations, maInvisibleTypeAnnotations)), maCode, (maVisibleAnnotations, maInvisibleAnnotations))
    = do
      ((parmst, maMethodParameters), (maVisibleParameterAnnotations, maInvisibleParameterAnnotations)) <-
        back handleParameters params
      ((desc, maExceptions), maSignatures) <- back handleSignature
                                                   (tps, parmst, rt, thrws)


      let maAnnotationDefault = []
      let maOthers            = []
      return (desc, B.MethodAttributes { .. })

  handleSignature
    :: Formatter
         ((B.MethodDescriptor, [B.Exceptions B.High]), [B.Signature B.High])
         ([TypeParameter], [Type], ReturnType, [ThrowsType])
  handleSignature =
    moreSignatureFormat
      . (   (inSecond $ compressList coerceFormat)
        *** ( isomap (textSerialize @B.MethodSignature . coerceFormat)
            . singletonList
            )
        )

  moreSignatureFormat
    :: Formatter
         ((B.MethodDescriptor, [ClassName]), Maybe B.MethodSignature)
         ([TypeParameter], [Type], ReturnType, [ThrowsType])
  moreSignatureFormat =
    withDefaultF
        (not . andOf
          (fold
            [ _1 . folded . like False
            , _2 . folded . to typeIsSimple
            , _3 . to returnTypeIsSimple
            , _4 . folded . to throwsTypeIsSimple
            ]
          )
        )
      . (unwrapMethodDescriptor *** isomap unwrapMethodSignature)

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

  handleParameters = parameterAnnotationsFormat . inFirst
    (parameterFormat . inSecond (isomap methodParametersFormat . singletonList))

  parameterFormat
    :: Formatter ([Type], Maybe [B.MethodParameter B.High]) [Parameter]
  parameterFormat =
    isomap
        (fromIso
          (\(tp, m) -> Parameter
            (m <&> \(B.MethodParameter n a) -> (n, B.toSet a))
            (withNoAnnotation tp)
            []
          )
          (\(Parameter nt tp _) ->
            ( view annotatedContent tp
            , nt <&> \(n, a) -> B.MethodParameter n (B.BitSet a)
            )
          )
        )
      . allOrNothing

parameterAnnotationsFormat
  :: Formatter
       ( [Parameter]
       , ( [B.RuntimeVisibleParameterAnnotations B.High]
         , [B.RuntimeInvisibleParameterAnnotations B.High]
         )
       )
       [Parameter]
parameterAnnotationsFormat = joinParameters . inSecond
  ( zipPadList (flipDirection $ partitionList (view annotationIsRuntimeVisible))
  . (   runtimeVisibleParameterAnnotationsFormat
    *** runtimeInvisibleParameterAnnotationsFormat
    )
  )
 where
  joinParameters :: Formatter ([Parameter], [Annotations]) [Parameter]
  joinParameters = PartIso
    (\(ps, an) -> case an of
      [] -> pure ps
      _
        | length ps == length an -> pure
        $ zipWith (\p a -> p & parameterAnnotations .~ a) ps an
        | otherwise -> Failure
          ["Annotation length different from number of parameters"]
    )
    (\p ->
      let x = map (view parameterAnnotations) p
      in  pure (p, if all null x then [] else x)
    )

  runtimeVisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeVisibleParameterAnnotations B.High] [Annotations]
  runtimeVisibleParameterAnnotationsFormat =
    isomap (annotationsFormat True) . compressList coerceFormat

  runtimeInvisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeInvisibleParameterAnnotations B.High] [Annotations]
  runtimeInvisibleParameterAnnotationsFormat =
    isomap (annotationsFormat False) . compressList coerceFormat

annotateMethodTypesFormat
  :: Formatter
       ( ([TypeParameter], [Parameter], ReturnType, [ThrowsType])
       , [(B.MethodTypeAnnotation B.High, (TypePath, Annotation))]
       )
       ( [Annotated TypeParameter]
       , [Parameter]
       , Annotated ReturnType
       , [Annotated ThrowsType]
       )
annotateMethodTypesFormat = PartIso anThere anBack where
  anThere ((tp, p, r, tt), mt) = do
    let ann = fmap reverse $ Map.fromListWith (++) (map (over _2 (: [])) mt)

    tpa <- validateEither . forM (zip tp [0 ..]) $ \(t, i) -> setTypeAnnotations
      (Map.findWithDefault [] (MethodTypeParameterDeclaration i) ann)
      (withNoAnnotation t)

    pa <- validateEither . forM (zip p [0 ..]) $ \(p', i) ->
      p' & parameterType %%~ setTypeAnnotations
        (Map.findWithDefault [] (MethodFormalParameter i) ann)

    ra <- validateEither $ setTypeAnnotations
      (Map.findWithDefault [] MethodReturnType ann)
      (withNoAnnotation r)

    tta <- validateEither . forM (zip tt [0 ..]) $ \(t', i) ->
      setTypeAnnotations (Map.findWithDefault [] (MethodThrowsClause i) ann)
                         (withNoAnnotation t')

    return (tpa, pa, ra, tta)

  anBack (tpa, pa, ra, tta) = return
    ( ( tpa ^.. folded . annotatedContent
      , pa
      , ra ^. annotatedContent
      , tta ^.. folded . annotatedContent
      )
    , concat
      [ concat
        [ map (MethodTypeParameterDeclaration i, ) (getTypeAnnotations t)
        | (i, t) <- zip [0 ..] tpa
        ]
      , concat
        [ map (MethodFormalParameter i, )
              (getTypeAnnotations . view parameterType $ t)
        | (i, t) <- zip [0 ..] pa
        ]
      , map (MethodReturnType, ) (getTypeAnnotations ra)
      , concat
        [ map (MethodThrowsClause i, ) (getTypeAnnotations t)
        | (i, t) <- zip [0 ..] tta
        ]
      ]
    )

 --  PartIso msThere msBack . fromLens _1 _1 moreSignatureFormat
 -- where

 --  msThere ((tp, prams, rt, thrws), pramsann, _) = do
 --    return
 --      ( map withNoAnnotation tp
 --      , zipWith
 --        (\p (a, mp) -> Parameter
 --          (fmap (\(B.MethodParameter name af) -> (name, B.toSet af)) mp)
 --          (withNoAnnotation p)
 --          a
 --        )
 --        prams
 --        pramsann
 --      , withNoAnnotation rt
 --      , map withNoAnnotation thrws
 --      )

 --  msBack (tps, prms, rt, thrs) = do
 --    return
 --      ( ( map (view annotatedContent)                   tps
 --        , map (view $ parameterType . annotatedContent) prms
 --        , view annotatedContent rt
 --        , map (view annotatedContent) thrs
 --        )
 --      , map
 --        (\p ->
 --          ( p ^. parameterAnnotations
 --          , fmap (\(a, b) -> B.MethodParameter a (B.BitSet b))
 --                 (p ^. parameterNameAndFlags)
 --          )
 --        )
 --        prms
 --      , []
 --      )



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
    codeAttributes' <- back codeAttributesFormat _codeStackMap
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
  ( withDefaultF (not . typeIsSimple)
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


annotateType
  :: forall a
   . (Show a, HasTypeAnnotations a)
  => Formatter ([(TypePath, Annotation)], a) (Annotated a)
annotateType = PartIso anThere anBack where

  anThere
    :: ([(TypePath, Annotation)], a) -> Validation [FormatError] (Annotated a)
  anThere (p, t) = validateEither
    (first ((show t ++ ": ") ++) $ setTypeAnnotations p (withNoAnnotation t))

  anBack
    :: Annotated a -> Validation [FormatError] ([(TypePath, Annotation)], a)
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
    _typeInterfaceBound <- mapM
      (fmap withNoAnnotation . there referenceTypeFromSignature)
      tpInterfaceBound
    _typeClassBound <- mapM
      (fmap withNoAnnotation . there referenceTypeFromSignature)
      tpClassBound
    let _typeIdentifier = tpIdentifier
    pure TypeParameter { .. }
  )
  (\TypeParameter {..} -> do
    tpInterfaceBound <- mapM
      (back referenceTypeFromSignature . view annotatedContent)
      _typeInterfaceBound
    tpClassBound <- mapM
      (back referenceTypeFromSignature . view annotatedContent)
      _typeClassBound
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
