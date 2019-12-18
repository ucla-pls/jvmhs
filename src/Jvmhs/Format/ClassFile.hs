{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : Jvmhs.Format.ClassFile
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describes conversion from @jvm-binary@'s 'ClassFile' format to
this Class format. Every variable in this module represents an partial iso-morphism
from the ClassFile format and back.

-}
module Jvmhs.Format.ClassFile where


-- lens
import           Control.Lens

-- jvm-binary
import qualified Language.JVM                  as B


import           Jvmhs.Data.Class
import           Jvmhs.Data.Annotation

-- | Validation is an Either which can collect it's faliures. This
-- is usefull when presenting this information to the user later.
data Validation e a
  = Success !a
  | Failure !e
  deriving (Eq, Ord, Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  fa <*> a = case fa of
    Success fn -> fn <$> a
    Failure e  -> case a of
      Success _  -> Failure e
      Failure e' -> Failure (e <> e')

instance Semigroup e => Monad (Validation e) where
  return = pure
  ma >>= fma = case ma of
    Success a -> fma a
    Failure e -> Failure e

-- | A Partial Isomorphism, we use them here allow convertion between the
-- two formats, while still perserving bugs. Every partial isomorphism
-- should be a valid adjunction.
-- This means that
-- >> there p . back p . there p == there p
data PartIso e a b = PartIso
  { there :: a -> Validation e b
  , back  :: b -> Validation e a
  }

-- | Change the direction of the partial isomorphism
flipDirection :: PartIso e a b -> PartIso e b a
flipDirection (PartIso t b) = (PartIso b t)

-- mk4 :: ((a, b, c, d) -> x) -> (x -> (a, b, c, d)) -> PartIso e (a, b, c, d) x
-- mk4 = undefined
-- 
-- mk5
--   :: ((a, b, c, d, e) -> x)
--   -> (x -> (a, b, c, d, e))
--   -> PartIso e (a, b, c, d, e) x
-- mk5 = undefined


type FormatError = String
type Formatter = PartIso [FormatError]

-- mkField
--   :: PartIso
--        e
--        (Text.Text, Type, Set.Set FAccessFlag, Maybe JValue, Annotations)
--        Field
-- mkField = mk5
--   (\(a, b, c, d, e) -> Field a b c d e)
--   (   (,,,,)
--   <$> (view fieldName)
--   <*> (view fieldType)
--   <*> (view fieldAccessFlags)
--   <*> (view fieldValue)
--   <*> (view fieldAnnotations)
--   )

--  { _fieldName        :: ! Text.Text
--  -- ^ the name of the field
--  , _fieldType        :: ! AnnotatedClassType
--  -- ^ the type of the field
--  , _fieldAccessFlags :: ! (Set.Set FAccessFlag)
--  -- ^ the set of access flags
--  , _fieldValue       :: ! (Maybe JValue)
--  -- ^ an optional value
--  , _fieldAnnotations :: Annotations
--  -- ^ the annotations of the feild

-- | Convert a `B.Field` to a `Field` 
fieldFormat :: Formatter (B.Field B.High) Field
fieldFormat = PartIso { there = fieldThere, back = fieldBack } where
  fieldThere f = do
    let _fieldName        = B.fName f
    let desc              = B.fDescriptor f
    let _fieldAccessFlags = B.fAccessFlags f

    (_fieldValue, _fieldAnnotations, _fieldType) <- there
      fieldAttributesFormat
      (desc, B.fAttributes f)

    pure Field { .. }

  fieldBack f = do
    let fName         = f ^. fieldName
    let fAccessFlags' = B.BitSet (f ^. fieldAccessFlags)

    (fDescriptor, fAttributes) <- back
      fieldAttributesFormat
      (f ^. fieldValue, f ^. fieldAnnotations, f ^. fieldType)

    pure B.Field { .. }


fieldAttributesFormat
  :: Formatter
       (B.FieldDescriptor, B.FieldAttributes B.High)
       (Maybe JValue, Annotations, Type)
fieldAttributesFormat = PartIso { there = attrThere, back = attrBack } where
  attrThere = undefined
  attrBack  = undefined

-- | An Isomorphism between classfiles and checked classes.
isoBinary :: Iso' (B.ClassFile B.High) Class
isoBinary = iso fromClassFile toClassFile

toClassFile :: Class -> B.ClassFile B.High
toClassFile = undefined

fromClassFile :: B.ClassFile B.High -> Class
fromClassFile = undefined




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

compress :: ([a] -> b) -> [a] -> [b]
compress f = \case
  [] -> []
  as -> [f as]
