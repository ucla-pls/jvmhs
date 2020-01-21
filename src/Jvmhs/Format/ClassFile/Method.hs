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
Module      : Jvmhs.Format.ClassFile.Method
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describes conversion from @jvm-binary@'s 'ClassFile' format to
this Class format.

-}
module Jvmhs.Format.ClassFile.Method where

-- base
import           Control.Category
import           Control.Monad
import           Data.Coerce
import           Data.Foldable
import           Data.Maybe
import           Prelude                 hiding ( id
                                                , (.)
                                                )

-- containers
import qualified Data.Map.Strict               as Map

-- lens
import           Control.Lens


-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B
import qualified Language.JVM.Attribute.Code   as B
import qualified Language.JVM.Attribute.Exceptions
                                               as B
import qualified Language.JVM.Attribute.MethodParameters
                                               as B
import qualified Language.JVM.Attribute.Signature
                                               as B

import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Identifier
import           Jvmhs.Data.Type

import           Jvmhs.Format.Internal
import           Jvmhs.Format.ClassFile.Shared
import           Jvmhs.Format.ClassFile.Type

methodFormat :: (ClassName -> Bool) -> Formatter (B.Method B.High) Method
methodFormat isStatic = PartIso methodThere methodBack where
  methodThere m = do
    let _methodName        = B.mName m
    let _methodAccessFlags = B.mAccessFlags m

    ((_methodTypeParameters, _methodParameters, _methodReturnType, _methodExceptions), _methodCode, _methodAnnotations) <-
      there (methodAttributesFormat isStatic) (B.mDescriptor m, B.mAttributes m)

    pure Method { .. }

  methodBack m = do
    let mName         = m ^. methodName
    let mAccessFlags' = B.BitSet (m ^. methodAccessFlags)

    (mDescriptor, mAttributes) <- back
      (methodAttributesFormat isStatic)
      ( ( m ^. methodTypeParameters
        , m ^. methodParameters
        , m ^. methodReturnType
        , m ^. methodExceptions
        )
      , m ^. methodCode
      , m ^. methodAnnotations
      )
    pure B.Method { .. }

methodParametersFormat
  :: Formatter (B.MethodParameters B.High) [B.MethodParameter B.High]
methodParametersFormat = coerceFormat
{-# INLINE methodParametersFormat #-}


methodAttributesFormat
  :: (ClassName -> Bool)
  -> Formatter
       (B.MethodDescriptor, B.MethodAttributes B.High)
       ( ( [Annotated TypeParameter]
         , [Parameter]
         , Annotated ReturnType
         , [Annotated ThrowsType]
         )
       , Maybe Code
       , Annotations
       )
methodAttributesFormat isStatic =
  triple (annotateMethodTypesFormat isStatic . inSecond typeAnnotationsFormat)
         (isomap codeFormat . singletonList)
         runtimeAnnotationsFormat
    . PartIso anThere anBack where

  anThere (desc, B.MethodAttributes {..}) = do
    (tps, parmst, rt, thrws) <- there handleSignature
                                      ((desc, maSignatures), maExceptions)

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
      ((desc, maSignatures), maExceptions) <- back handleSignature
                                                   (tps, parmst, rt, thrws)


      let maAnnotationDefault = []
      let maOthers            = []
      return (desc, B.MethodAttributes { .. })

  handleSignature
    :: Formatter
         ((B.MethodDescriptor, [B.Signature B.High]), [B.Exceptions B.High])
         ([TypeParameter], [(Bool, Type)], ReturnType, [ThrowsType])
  handleSignature =
    methodSignatureFormat
      . (   inSecond
            ( isomap (textSerialize @B.MethodSignature . coerceFormat)
            . singletonList
            )
        *** compressList coerceFormat
        )

  handleParameters = parameterAnnotationsFormat . inFirst
    (parameterFormat . inSecond (isomap methodParametersFormat . singletonList))

  parameterFormat
    :: Formatter ([(Bool, Type)], Maybe [B.MethodParameter B.High]) [Parameter]
  parameterFormat =
    isomap
        (fromIso
          (\((b, tp), m) -> Parameter
            (m <&> \(B.MethodParameter n a) -> (n, B.toSet a))
            b
            (withNoAnnotation tp)
            []
          )
          (\(Parameter nt b tp _) ->
            ( (b, view annotatedContent tp)
            , nt <&> \(n, a) -> B.MethodParameter n (B.BitSet a)
            )
          )
        )
      . allOrNothing

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

methodSignatureFormat
  :: Formatter
       ((B.MethodDescriptor, Maybe B.MethodSignature), [ClassName])
       ([TypeParameter], [(Bool, Type)], ReturnType, [ThrowsType])
methodSignatureFormat = addThrows
  . inFirst (joinem . inSecond (isomap unwrapMethodSignature))
 where
  joinem
    :: Formatter
         ( B.MethodDescriptor
         , Maybe ([TypeParameter], [Type], ReturnType, [ThrowsType])
         )
         ([TypeParameter], [(Bool, Type)], ReturnType, [ThrowsType])
  joinem = PartIso maThere maBack   where
    maThere (B.MethodDescriptor {..}, sig) = case sig of
      Just (tpm, tps, rt, thrws) -> do
        tps' <-
          case
            sequence
              (zipWith bindType
                       (reverse methodDescriptorArguments)
                       (reverse tps)
              )
          of
            Just tps' -> Success
              (  [ (False, fromJType t)
                 | t <- take (length methodDescriptorArguments - length tps)
                             methodDescriptorArguments
                 ]
              ++ (map (True, ) $ reverse tps')
              )
            Nothing -> Failure ["Signature and parameters does not match"]
        rt' <-
          case bindReturnType (B.asMaybeJType methodDescriptorReturnType) rt of
            Just rt' -> Success rt'
            Nothing  -> Failure
              [ "Signature and return type does not match: "
                ++ show methodDescriptorReturnType
                ++ " =/= "
                ++ show rt
              ]

        pure (tpm, tps', rt', thrws)
      Nothing -> pure
        ( []
        , map ((True, ) . fromJType) methodDescriptorArguments
        , ReturnType
          (fmap fromJType (B.asMaybeJType methodDescriptorReturnType))
        , []
        )

    maBack (tpm, tps, rt, thrws) = do
      let methodDescriptorArguments = map (toBoundJType . snd) tps
      let methodDescriptorReturnType =
            B.ReturnDescriptor (fmap toBoundJType . view returnType $ rt)
      pure
        ( B.MethodDescriptor { .. }
        , if null tpm
             && all (typeIsSimple . snd) tps
             && all fst                  tps
             && returnTypeIsSimple rt
             && all throwsTypeIsSimple thrws
          then Nothing
          else Just (tpm, map snd (filter fst tps), rt, thrws)
        )

  addThrows
    :: Formatter
         ( ([TypeParameter], [(Bool, Type)], ReturnType, [ThrowsType])
         , [ClassName]
         )
         ([TypeParameter], [(Bool, Type)], ReturnType, [ThrowsType])
  addThrows = PartIso aThere aBack   where
    aThere (a, c) = _4
      (\case
        [] -> pure $ map (ThrowsClass . classTypeFromName) c
        as -> if length as /= length c
          then Failure ["Incompatable lenghts of exceptions"]
          else case sequence $ zipWith (\cn x' -> bindThrowsType cn x') c as of
            Just r  -> Success r
            Nothing -> Failure ["Signature and exception list does not match"]
      )
      a
    aBack a = pure
      ( a
      &  _4
      %~ (\z -> if andOf (folded . to throwsTypeIsSimple) z then [] else z)
      , map boundClassNameFromThrowsType (a ^. _4)
      )

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
        | length an <= length ps -> pure . reverse $ zipWith
          (\p a -> p & parameterAnnotations .~ a)
          (reverse ps)
          (reverse an ++ repeat [])
        | otherwise -> Failure
          [ "Annotation length must be smaller or equal to the number of parameters"
          ]
    )
    (\p ->
      let x =
              map (view parameterAnnotations) . filter (view parameterVisible) $ p
      in  pure (p, x)
    )

  runtimeVisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeVisibleParameterAnnotations B.High] [Annotations]
  runtimeVisibleParameterAnnotationsFormat =
    isomap (annotationsFormat True) . compressDeepList coerceFormat

  runtimeInvisibleParameterAnnotationsFormat
    :: Formatter [B.RuntimeInvisibleParameterAnnotations B.High] [Annotations]
  runtimeInvisibleParameterAnnotationsFormat =
    isomap (annotationsFormat False) . compressDeepList coerceFormat


  compressDeepList :: Formatter a [[b]] -> Formatter [a] [[b]]
  compressDeepList (PartIso f t) = PartIso
    (fmap concat . mapM f)
    (\case
      bs | all null bs -> pure []
         | otherwise   -> (: []) <$> t bs
    )

annotateMethodTypesFormat
  :: (ClassName -> Bool)
  -> Formatter
       ( ([TypeParameter], [Parameter], ReturnType, [ThrowsType])
       , [(B.MethodTypeAnnotation B.High, (TypePath, Annotation))]
       )
       ( [Annotated TypeParameter]
       , [Parameter]
       , Annotated ReturnType
       , [Annotated ThrowsType]
       )
annotateMethodTypesFormat isStatic = PartIso anThere anBack where
  anThere ((tp, p, r, tt), mt) = do
    let ann = fmap reverse $ Map.fromListWith (++) (map (over _2 (: [])) mt)

    tpa <- validateEither . forM (zip tp [0 ..]) $ \(t, i) -> setTypeAnnotations
      isStatic
      (Map.findWithDefault [] (MethodTypeParameterDeclaration i) ann)
      (withNoAnnotation t)

    pa <- validateEither . forM (zip p [0 ..]) $ \(p', i) ->
      p' & parameterType %%~ setTypeAnnotations
        isStatic
        (Map.findWithDefault [] (MethodFormalParameter i) ann)

    ra <- validateEither $ setTypeAnnotations
      isStatic
      (Map.findWithDefault [] MethodReturnType ann)
      (withNoAnnotation r)

    tta <- validateEither . forM (zip tt [0 ..]) $ \(t', i) ->
      setTypeAnnotations isStatic
                         (Map.findWithDefault [] (MethodThrowsClause i) ann)
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
        [ map (MethodThrowsClause i, ) (getTypeAnnotations isStatic t)
        | (i, t) <- zip [0 ..] tta
        ]
      , concat
        [ map (MethodTypeParameterDeclaration i, )
              (getTypeAnnotations isStatic t)
        | (i, t) <- zip [0 ..] tpa
        ]
      , map (MethodReturnType, ) (getTypeAnnotations isStatic ra)
      , concat
        [ map (MethodFormalParameter i, )
              (getTypeAnnotations isStatic . view parameterType $ t)
        | (i, t) <- zip [0 ..] pa
        ]
      ]
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
