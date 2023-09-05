{-# LANGUAGE BlockArguments #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Jvmhs.Format.Json where

-- base
import Data.Foldable

-- aeson
import Data.Aeson hiding (parseJSONList, toJSONList)

-- lens
import Control.Lens hiding ((.=))

-- containers
import qualified Data.Set as Set

-- text
import qualified Data.Text as Text

-- jvm-binary

import qualified Language.JVM as B
import qualified Language.JVM as JVM
import qualified Language.JVM.Attribute.Annotations as B

-- jvmhs
import Jvmhs.Data.Class
import Jvmhs.Data.Identifier
import Jvmhs.Data.Type

import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (JSONPathElement (Index, Key), Parser, listParser, listValue)
import qualified Data.Vector as V
import Debug.Trace (traceM)
import Jvmhs.Data.Code
import Jvmhs.Format.JsonTH

{-

How do we format json?

-}

$( makeJson
    [TA ''ClassName]
 )

toJSONOptional :: (a -> Value) -> Maybe a -> Value
toJSONOptional f =
  liftToJSON
    f
    (error "Unexpected")

parseJSONOptional :: (Value -> Parser a) -> Value -> Parser (Maybe a)
parseJSONOptional f =
  liftParseJSON f (error "Unexpected")

toJSONList :: (a -> Value) -> [a] -> Value
toJSONList = listValue

parseJSONList :: (Value -> Parser a) -> Value -> Parser [a]
parseJSONList f =
  withArray "Array" $
    fmap V.toList . V.imapM (\i a -> f a <?> Index i)

instance ToJSON AnnotationValue
instance FromJSON AnnotationValue

instance ToJSON (B.EnumValue B.High) where
  toJSON c = case c of
    B.EnumValue{..} ->
      Object $
        fold
          [ "type" .= enumTypeName
          , "const" .= enumConstName
          ]

instance FromJSON (B.EnumValue B.High) where
  parseJSON = withObject "EnumValue" $ \c ->
    B.EnumValue <$> c .: "type" <*> c .: "const"

$( makeJson
    [ TS
        ''Annotation
        [ F "type" '_annotationType ["!"] ""
        , F "runtime_visible" '_annotationIsRuntimeVisible ["!"] ""
        , F "values" '_annotationValues ["!"] ""
        ]
    ]
 )

toJSONAnnotated :: (a -> Value) -> Annotated a -> Value
toJSONAnnotated f = \case
  Annotated a [] ->
    f a
  Annotated a annos ->
    let anns = "annotations" .= toJSONList toJSONAnnotation annos
     in case f a of
          Object m -> Object (m <> anns)
          a' -> Object $ "content" .= a' <> anns

parseJSONAnnotated :: Show a => (Value -> Parser a) -> Value -> Parser (Annotated a)
parseJSONAnnotated f v = case v of
  Object o ->
    case KM.lookup "annotations" o of
      Just x -> do
        traceM ("Annotations found" <> show x)
        a <- parseJSONList parseJSONAnnotation x
        c' <- msum [o .: "content", pure v, fail "bad"]
        traceM ("content: " <> show c')
        c <- f c'
        traceM ("result: " <> show c)
        pure $ Annotated c a
      Nothing -> do
        traceM ("Annotations not found" <> show o)
        c <- f v
        pure $ Annotated c []
  _ -> do
    c <- f v
    pure $ Annotated c []

-- msum
--   [ v & withObject "Annotated" \o -> do
--       a <-
--         maybe (fail "annotations") (parseJSONList parseJSONAnnotation) $
--           KM.lookup "annotations" o
--       c <- msum [f =<< o .: "content", f v]
--       pure $ Annotated c a
--   , do
-- [ do
--     c <- f v
--     pure $ Annotated c []
-- ]

instance ToJSON BootstrapMethod
instance FromJSON BootstrapMethod where
  parseJSON _ = fail "Cannot parse bootstrap methods yet"

$( makeJson
    [ TS
        ''ClassType
        [ F "name" '_classTypeName ["!"] ""
        , F "inner" '_classTypeInner ["Optional", "Annotated"] ""
        , F "arguments" '_classTypeArguments ["List", "Annotated"] ""
        ]
    , TS
        ''TypeVariable
        [ F "name" '_typeVariableName [] ""
        , F "bound" '_typeVariableBound ["!"] ""
        ]
    , TA ''TypeVariableName
    ]
 )

toJSONReturnType :: ReturnType -> Value
toJSONReturnType (ReturnType t) = toJSONOptional toJSONType t

parseJSONReturnType :: Value -> Parser ReturnType
parseJSONReturnType v = ReturnType <$> parseJSONOptional parseJSONType v

toJSONJBaseType :: JBaseType -> Value
toJSONJBaseType b = String (Text.pack [JVM.jBaseTypeToChar b])

parseJSONJBaseType :: Value -> Parser JBaseType
parseJSONJBaseType = withText "JBaseType" \s -> case Text.unpack s of
  ['B'] -> return JTByte
  ['C'] -> return JTChar
  ['D'] -> return JTDouble
  ['F'] -> return JTFloat
  ['I'] -> return JTInt
  ['J'] -> return JTLong
  ['S'] -> return JTShort
  ['Z'] -> return JTBoolean
  s' -> fail $ "Unknown type " <> show s'

toJSONTypeArgument :: TypeArgument -> Value
toJSONTypeArgument _ = Null

parseJSONTypeArgument :: Value -> Parser TypeArgument
parseJSONTypeArgument _ = fail "Unimplemented"

toJSONType :: Type -> Value
toJSONType = \case
  ReferenceType t -> toJSONReferenceType t
  BaseType b -> toJSONJBaseType b

parseJSONType :: Value -> Parser Type
parseJSONType v =
  msum
    [ BaseType <$> parseJSONJBaseType v
    , ReferenceType <$> parseJSONReferenceType v
    ]

toJSONReferenceType :: ReferenceType -> Value
toJSONReferenceType = \case
  RefClassType ct -> toJSONClassType ct
  RefTypeVariable tv -> toJSONTypeVariable tv
  RefArrayType (ArrayType ct) -> toJSONAnnotated toJSONType ct

parseJSONReferenceType :: Value -> Parser ReferenceType
parseJSONReferenceType v =
  msum
    [ RefClassType <$> parseJSONClassType v
    , RefTypeVariable <$> parseJSONTypeVariable v
    , fail "recursion" -- RefArrayType . ArrayType <$> parseJSONAnnotated parseJSONType v
    ]

toJSONThrowsType :: ThrowsType -> Value
toJSONThrowsType = \case
  ThrowsClass ct -> toJSONClassType ct
  ThrowsTypeVariable tv -> toJSONTypeVariable tv

parseJSONThrowsType :: Value -> Parser ThrowsType
parseJSONThrowsType v =
  msum
    [ ThrowsClass <$> parseJSONClassType v
    , ThrowsTypeVariable <$> parseJSONTypeVariable v
    ]

$( makeJson
    [ TS
        ''Class
        [ F "name" '_className' [] ""
        , F "access" '_classAccessFlags ["!"] ""
        , F "type_params" '_classTypeParameters ["List", "Annotated"] ""
        , F "super" '_classSuper ["Optional", "Annotated"] ""
        , F "interfaces" '_classInterfaces ["List", "Annotated"] ""
        , F "fields" '_classFields ["List"] ""
        , F "methods" '_classMethods ["List"] ""
        , F "bootstrap_methods" '_classBootstrapMethods ["!"] ""
        , F "enclosing_method" '_classEnclosingMethod ["!"] ""
        , F "inner_classes" '_classInnerClasses ["List"] ""
        , F "annotations" '_classAnnotations ["List", "Annotation"] ""
        , F "version" '_classVersion ["!"] ""
        ]
    , TS
        ''Method
        [ F "name" '_methodName ["!"] ""
        , F "type_parameters" '_methodTypeParameters ["List", "Annotated"] ""
        , F "parameters" '_methodParameters ["List", "Annotated"] ""
        , F "returns" '_methodReturnType ["Annotated"] ""
        , F "access" '_methodAccessFlags ["!"] ""
        , F "exceptions" '_methodExceptions ["List", "Annotated"] ""
        , F "code" '_methodCode ["Optional"] ""
        , F "annotations" '_methodAnnotations ["List", "Annotation"] ""
        , F "default" '_methodDefaultAnnotation ["Optional", "!"] ""
        ]
    , TS
        ''Field
        [ F "name" '_fieldName ["!"] ""
        , F "type" '_fieldType ["Annotated"] ""
        , F "access" '_fieldAccessFlags ["!"] ""
        , F "value" '_fieldValue ["Optional", "!"] ""
        , F "annotations" '_fieldAnnotations ["List", "Annotation"] ""
        ]
    , TS
        ''TypeParameter
        [ F "name" '_typeParameterName [] ""
        , F "class_bound" '_typeParameterClassBound ["Optional", "Annotated"] ""
        , F "interface_bound" '_typeParameterInterfaceBound ["List", "Annotated"] ""
        ]
    , TS
        ''Parameter
        [ F "name" '_parameterNameAndFlags ["!"] ""
        , F "type" '_parameterType ["Annotated"] ""
        , F "visible" '_parameterVisible ["!"] ""
        ]
    , TS
        ''InnerClass
        [ F "name" '_innerClass [] ""
        , F "outer" '_innerOuterClass ["Optional"] ""
        , F "inner" '_innerClassName ["Optional", "!"] ""
        , F "access" '_innerAccessFlags ["!"] ""
        ]
    , TS
        ''Code
        [ F "max_stack" '_codeMaxStack ["!"] ""
        , F "max_locals" '_codeMaxLocals ["!"] ""
        , F "exceptions" '_codeExceptionTable ["List"] ""
        , F "stack_map" '_codeStackMap ["Optional", "!"] ""
        , F "byte_code" '_codeByteCode ["!"] ""
        ]
    , TS
        ''ExceptionHandler
        [ F "start" '_ehStart ["!"] ""
        , F "end" '_ehEnd ["!"] ""
        , F "handler" '_ehHandler ["!"] ""
        , F "catch_type" '_ehCatchType ["Optional"] ""
        ]
    ]
 )

--   toJSON c =
--     Object $
--       fold
--         [ "name" .= (c ^. className)
--         , "access" .= (c ^. classAccessFlags)
--         , "type_param" .= (c ^. classTypeParameters)
--         , "super" .= (c ^. classSuper)
--         , "interfaces" .= (c ^. classInterfaces)
--         , "fields" .= (c ^. classFields)
--         , "methods" .= (c ^. classMethods)
--         , "bootstrap_methods" .= (c ^. classBootstrapMethods)
--         , "enclosing" .= case c ^. classEnclosingMethod of
--             Just (cls, mm) -> Object ("class" .= cls <> "method" .= mm)
--             Nothing -> Null
--         , "inner_classes" .= (c ^. classInnerClasses)
--         , "annotations" .= (c ^. classAnnotations)
--         , "version" .= (c ^. classVersion)
--         ]
--
-- instance ToJSON Field
--
-- instance ToJSON Method
--
-- instance ToJSON BootstrapMethod
--
-- instance ToJSON ReturnType
-- instance ToJSON Parameter
-- instance ToJSON PAccessFlag
--
-- instance ToJSON ThrowsType
-- instance ToJSON TypeVariable
-- instance ToJSON ClassType
-- instance ToJSON TypeArgument
-- instance ToJSON ReferenceType
-- instance ToJSON ArrayType
-- instance ToJSON Type
-- instance ToJSON JBaseType
-- instance ToJSON InnerClass
