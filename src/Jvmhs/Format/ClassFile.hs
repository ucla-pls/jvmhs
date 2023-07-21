{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Jvmhs.Format.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describes conversion from @jvm-binary@'s 'ClassFile' format to
this Class format.
-}
module Jvmhs.Format.ClassFile (
  fromClassFile,
  toClassFile,

  -- * Lowerlevel formaters
  classFormat,
  classAttributeFormat,
  fieldFormat,
  fieldAttributesFormat,
  fieldTypeFormat,
  module Jvmhs.Format.Internal,
  module Jvmhs.Format.ClassFile.Shared,
  module Jvmhs.Format.ClassFile.Type,
  module Jvmhs.Format.ClassFile.Method,
) where

-- base
import Control.Category
import Control.Monad
import Data.Coerce
import Data.Maybe
import Prelude hiding (
  id,
  (.),
 )

-- containers

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- lens
import Control.Lens

-- jvm-binary
import qualified Language.JVM as B
import qualified Language.JVM.Attribute.Annotations as B
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM.Attribute.EnclosingMethod as B
import qualified Language.JVM.Attribute.InnerClasses as B
import qualified Language.JVM.Attribute.Signature as B

import Jvmhs.Data.Class
import Jvmhs.Data.Identifier
import Jvmhs.Data.Type

import Jvmhs.Format.ClassFile.Method
import Jvmhs.Format.ClassFile.Shared
import Jvmhs.Format.ClassFile.Type
import Jvmhs.Format.Internal

toClassFile :: Class -> Either [FormatError] (B.ClassFile B.High)
toClassFile = runValidation . back classFormat

fromClassFile :: B.ClassFile B.High -> Either [FormatError] Class
fromClassFile = runValidation . there classFormat

classFormat :: Formatter (B.ClassFile B.High) Class
classFormat =
  PartIso
    ( \B.ClassFile{..} -> do
        let _classAccessFlags = coerce cAccessFlags'
        let _className' = cThisClass
        let _classVersion = Just (cMajorVersion, cMinorVersion)

        ((_classSuper', _classInterfaces, _classTypeParameters), (_classBootstrapMethods', _classEnclosingMethod, _classInnerClasses), _classAnnotations) <-
          there classAttributeFormat ((cSuperClass, cInterfaces), cAttributes)

        let staticSet = staticInnerClasses _classInnerClasses
            isStatic = (`Set.member` staticSet)
            _classBootstrapMethods =
              IntMap.fromAscList $ zip [0 ..] _classBootstrapMethods'

        _classFields <-
          there
            (coerceFormat . isomap (fieldFormat isStatic))
            cFields'

        _classMethods <-
          there
            (coerceFormat . isomap (methodFormat isStatic))
            cMethods'

        let _classSuper =
              if _className' == "java/lang/Object"
                then Nothing
                else Just _classSuper'

        pure Class{..}
    )
    ( \(reindexBootstrapMethods -> (bootstrapMethods, Class{..})) -> do
        let cMagicNumber = 0xCAFEBABE
        let cConstantPool = ()
        let cAccessFlags' = B.BitSet _classAccessFlags
        let cThisClass = _className'
        let (cMajorVersion, cMinorVersion) = fromMaybe (52, 0) _classVersion

        let staticSet = staticInnerClasses _classInnerClasses
            isStatic = (`Set.member` staticSet)

        cFields' <- back (coerceFormat . isomap (fieldFormat isStatic)) _classFields
        cMethods' <-
          back
            (coerceFormat . isomap (methodFormat isStatic))
            _classMethods

        ((cSuperClass, cInterfaces), cAttributes) <-
          back
            classAttributeFormat
            (
              ( fromMaybe
                  (withNoAnnotation (classTypeFromName "java/lang/Object"))
                  _classSuper
              , _classInterfaces
              , _classTypeParameters
              )
            , (bootstrapMethods, _classEnclosingMethod, _classInnerClasses)
            , _classAnnotations
            )

        pure B.ClassFile{..}
    )

classAttributeFormat ::
  Formatter
    ((ClassName, B.SizedList16 ClassName), B.ClassAttributes B.High)
    ( (Annotated ClassType, [Annotated ClassType], [Annotated TypeParameter])
    , ([BootstrapMethod], Maybe (ClassName, Maybe MethodId), [InnerClass])
    , Annotations
    )
classAttributeFormat =
  helper
    . fromIso
      ( \((sc, itf), (s, bs, em, ic, tann, an)) ->
          ((((sc, itf), s), tann), (bs, em, ic), an)
      )
      ( \((((sc, itf), s), tann), (bs, em, ic), an) ->
          ((sc, itf), (s, bs, em, ic, tann, an))
      )
    . inSecond unwrapClassAttributes
 where
  helper =
    PartIso
      ( \(a, b, c) -> do
          b' <- there classExtras b

          let staticSet = staticInnerClasses (b' ^. _3)
              isStatic = (`Set.member` staticSet)

          a' <- there (classSignatureFormat isStatic) a

          c' <- there runtimeAnnotationsFormat c
          pure (a', b', c')
      )
      ( \(a', b', c') -> do
          b <- back classExtras b'

          let staticSet = staticInnerClasses (b' ^. _3)
              isStatic = (`Set.member` staticSet)

          a <- back (classSignatureFormat isStatic) a'
          c <- back runtimeAnnotationsFormat c'

          pure (a, b, c)
      )

  unwrapClassAttributes =
    fromIso
      ( \B.ClassAttributes{..} ->
          ( caSignature
          , caBootstrapMethods
          , caEnclosingMethod
          , caInnerClasses
          , (caVisibleTypeAnnotations, caInvisibleTypeAnnotations)
          , (caVisibleAnnotations, caInvisibleAnnotations)
          )
      )
      ( \(caSignature, caBootstrapMethods, caEnclosingMethod, caInnerClasses, (caVisibleTypeAnnotations, caInvisibleTypeAnnotations), (caVisibleAnnotations, caInvisibleAnnotations)) ->
          let caOthers = [] in B.ClassAttributes{..}
      )

  classSignatureFormat ::
    (ClassName -> Bool) ->
    Formatter
      ( ((ClassName, B.SizedList16 ClassName), [B.Signature B.High])
      , ( [B.RuntimeVisibleTypeAnnotations B.ClassTypeAnnotation B.High]
        , [B.RuntimeInvisibleTypeAnnotations B.ClassTypeAnnotation B.High]
        )
      )
      ( Annotated ClassType
      , [Annotated ClassType]
      , [Annotated TypeParameter]
      )
  classSignatureFormat isStatic =
    annotateClass isStatic
      . ( withDefaultF
            ( not
                . andOf
                  ( (_1 . to classTypeIsSimple)
                      <> (_2 . folded . to classTypeIsSimple)
                      <> (_3 . folded . like False)
                  )
            )
            . ( fromIso
                  ( \(cn, lst) ->
                      (classTypeFromName cn, map classTypeFromName $ coerce lst, [])
                  )
                  ( \(ct, intf, _) ->
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

  classSignatureFormat' ::
    Formatter B.ClassSignature (ClassType, [ClassType], [TypeParameter])
  classSignatureFormat' =
    triple classTypeFormat (isomap classTypeFormat) (isomap typeParameterFormat)
      . mkBClassSignature

  mkBClassSignature =
    fromIso
      ( \B.ClassSignature{..} ->
          (csSuperclassSignature, csInterfaceSignatures, csTypeParameters)
      )
      ( \(csSuperclassSignature, csInterfaceSignatures, csTypeParameters) ->
          B.ClassSignature{..}
      )

  classExtras ::
    Formatter
      ( [B.BootstrapMethods B.High]
      , [B.EnclosingMethod B.High]
      , [B.InnerClasses B.High]
      )
      ([BootstrapMethod], Maybe (ClassName, Maybe MethodId), [InnerClass])
  classExtras =
    triple
      (compressList bootstrapMethodsFormat)
      ( isomap
          ( fromIso
              (\(B.EnclosingMethod cn mid) -> (cn, mid))
              (\(cn, mid) -> B.EnclosingMethod cn mid)
          )
          . singletonList
      )
      (compressList innerClassesFormat)

  -- TODO: More work needed
  annotateClass ::
    (ClassName -> Bool) ->
    Formatter
      ( (ClassType, [ClassType], [TypeParameter])
      , [(B.ClassTypeAnnotation B.High, (TypePath, Annotation))]
      )
      ( Annotated ClassType
      , [Annotated ClassType]
      , [Annotated TypeParameter]
      )
  annotateClass isStatic = PartIso acThere acBack
   where
    acThere ((ct, inn, tp), ctan) = do
      let ann =
            fmap reverse . Map.fromListWith (++) . map (over _2 (: [])) $ ctan

      act <-
        validateEither $
          setTypeAnnotations
            isStatic
            (Map.findWithDefault [] (ClassSuperType 65535) ann)
            (withNoAnnotation ct)

      ainn <- validateEither . forM (zip inn [0 ..]) $ \(ii, i) ->
        setTypeAnnotations
          isStatic
          (Map.findWithDefault [] (ClassSuperType i) ann)
          (withNoAnnotation ii)

      atp <- validateEither . forM (zip tp [0 ..]) $ \(t', i) -> do
        a <-
          setTypeAnnotations
            isStatic
            (Map.findWithDefault [] (ClassTypeParameterDeclaration i) ann)
            (withNoAnnotation t')

        a' <-
          a
            & annotatedContent
              . typeParameterClassBound
              . _Just
            %%~ setTypeAnnotations
              isStatic
              ( Map.findWithDefault
                  []
                  ( B.ClassBoundTypeParameterDeclaration
                      (B.TypeParameterBoundTarget i 0)
                  )
                  ann
              )

        a'
          & annotatedContent
            . typeParameterInterfaceBound
          %%~ ( \inbs -> forM (zip inbs [1 ..]) $ \(t'', j) -> do
                  setTypeAnnotations
                    isStatic
                    ( Map.findWithDefault
                        []
                        ( B.ClassBoundTypeParameterDeclaration
                            (B.TypeParameterBoundTarget i j)
                        )
                        ann
                    )
                    t''
              )

      pure (act, ainn, atp)

    acBack (ct, inn, tp) = do
      pure
        (
          ( view annotatedContent ct
          , map (view annotatedContent) inn
          , map (view annotatedContent) tp
          )
        , concat
            [ map (ClassSuperType 65535,) $ getTypeAnnotations isStatic ct
            , concat
                [ map (ClassSuperType i,) $ getTypeAnnotations isStatic t
                | (i, t) <- zip [0 ..] inn
                ]
            , concat
                [ concat
                  [ map
                      (ClassTypeParameterDeclaration i,)
                      (getTypeAnnotations isStatic t)
                  , concat
                      [ map
                        (ClassBoundTypeParameterDeclaration
                          (B.TypeParameterBoundTarget i 0),)
                        (getTypeAnnotations isStatic t')
                      | t' <-
                          t ^.. annotatedContent . typeParameterClassBound . _Just
                      ]
                  , concat
                      [ map
                        (ClassBoundTypeParameterDeclaration
                          (B.TypeParameterBoundTarget i j),)
                        (getTypeAnnotations isStatic t')
                      | (j, t') <-
                          zip
                            [1 ..]
                            (t ^. annotatedContent . typeParameterInterfaceBound)
                      ]
                  ]
                | (i, t) <- zip [0 ..] tp
                ]
            ]
        )

bootstrapMethodsFormat ::
  Formatter (B.BootstrapMethods B.High) [BootstrapMethod]
bootstrapMethodsFormat =
  isomap
    ( fromIso
        (\(B.BootstrapMethod a b) -> BootstrapMethod a (coerce b))
        (\(BootstrapMethod a b) -> B.BootstrapMethod a (coerce b))
    )
    . coerceFormat
{-# INLINE bootstrapMethodsFormat #-}

innerClassesFormat :: Formatter (B.InnerClasses B.High) [InnerClass]
innerClassesFormat =
  isomap
    ( fromIso
        ( \B.InnerClass{..} ->
            InnerClass
              { _innerClass = icClassName
              , _innerOuterClass = icOuterClassName
              , _innerClassName = icInnerName
              , _innerAccessFlags = B.toSet icInnerAccessFlags
              }
        )
        ( \InnerClass{..} ->
            B.InnerClass
              { icClassName = _innerClass
              , icOuterClassName = _innerOuterClass
              , icInnerName = _innerClassName
              , icInnerAccessFlags = B.BitSet _innerAccessFlags
              }
        )
    )
    . coerceFormat

-- | Convert a `B.Field` to a `Field`
fieldFormat :: (ClassName -> Bool) -> Formatter (B.Field B.High) Field
fieldFormat isStatic = PartIso{there = fieldThere, back = fieldBack}
 where
  fieldThere f = do
    let _fieldName = B.fName f
    let desc = B.fDescriptor f
    let _fieldAccessFlags = B.fAccessFlags f

    ((_fieldValue, _fieldAnnotations), _fieldType) <-
      there
        (fieldAttributesFormat isStatic)
        (desc, B.fAttributes f)

    pure Field{..}

  fieldBack f = do
    let fName = f ^. fieldName
    let fAccessFlags' = B.BitSet (f ^. fieldAccessFlags)

    (fDescriptor, fAttributes) <-
      back
        (fieldAttributesFormat isStatic)
        ((f ^. fieldValue, f ^. fieldAnnotations), f ^. fieldType)

    pure B.Field{..}

fieldTypeFormat ::
  (ClassName -> Bool) ->
  Formatter
    ([(TypePath, Annotation)], (B.FieldDescriptor, [B.Signature B.High]))
    (Annotated Type)
fieldTypeFormat isStatic =
  annotateType isStatic
    . inSecond (joinem . ((coerceFormat) *** fieldTypeFromSignature))
 where
  joinem :: Formatter (B.JType, Maybe Type) Type
  joinem = PartIso pThere pBack
   where
    pThere (rf, mt) = case mt of
      Just n -> case bindType rf n of
        Just n' -> pure n'
        Nothing -> Failure ["Field type does not match signature"]
      Nothing -> pure $ fromJType rf
    pBack t =
      pure (t ^. simpleType, if typeIsSimple t then Nothing else Just t)

  fieldTypeFromSignature :: Formatter [B.Signature B.High] (Maybe Type)
  fieldTypeFromSignature =
    (flipDirection $ fromPrism _ReferenceType)
      . isomap (referenceTypeFromSignature . signatureFormat)
      . singletonList

  signatureFormat :: Formatter (B.Signature B.High) B.ReferenceType
  signatureFormat =
    coerceFormat . textSerialize @B.FieldSignature . coerceFormat

fieldAttributesFormat ::
  (ClassName -> Bool) ->
  Formatter
    (B.FieldDescriptor, B.FieldAttributes B.High)
    ((Maybe JValue, Annotations), Annotated Type)
fieldAttributesFormat isStatic =
  inSecond (fieldTypeFormat isStatic) . PartIso attrThere attrBack
 where
  attrThere (fdesc, fattr) = do
    mjv <- there constantValueFormat (B.faConstantValues fattr)
    ann <-
      there
        runtimeAnnotationsFormat
        (B.faVisibleAnnotations fattr, B.faInvisibleAnnotations fattr)

    tpann <-
      there
        (typeAnnotationsFormat @B.FieldTypeAnnotation)
        (B.faVisibleTypeAnnotations fattr, B.faInvisibleTypeAnnotations fattr)

    pure ((mjv, ann), (map (view _2) tpann, (fdesc, B.faSignatures fattr)))

  attrBack ((mjv, ann), (tpann, (fdesc, faSignatures))) = do
    faConstantValues <- back constantValueFormat mjv

    (faVisibleAnnotations, faInvisibleAnnotations) <-
      back
        runtimeAnnotationsFormat
        ann

    (faVisibleTypeAnnotations, faInvisibleTypeAnnotations) <-
      back
        (typeAnnotationsFormat @B.FieldTypeAnnotation)
        (map (B.FieldTypeAnnotation,) tpann)

    let faOthers = []

    pure (fdesc, B.FieldAttributes{..})
