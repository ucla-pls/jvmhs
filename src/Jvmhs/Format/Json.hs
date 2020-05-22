{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-imports #-}
module Jvmhs.Format.Json where

-- base
import Data.Foldable

-- aeson
import           Data.Aeson

-- lens
import           Control.Lens hiding ((.=))

-- containers
import qualified Data.Set                      as Set

-- text
import qualified Data.Text                     as Text

-- jvm-binary
import qualified Language.JVM as JVM

-- jvmhs
import           Jvmhs.Data.Class
import           Jvmhs.Data.Type
import           Jvmhs.Data.Identifier

{-

How do we format json?

-}

instance ToJSON a => ToJSON (Annotated a) where
  toJSON (Annotated a annos) = case annos of 
    [] -> toJSON a
    _ -> Object $ 
      "content" .= a <> "annotations" .= annos

instance ToJSON Class where
  toJSON c = Object $ fold
    [ "name" .= (c ^. className)
    , "access" .= (c ^. classAccessFlags)
    , "type_param" .= (c ^. classTypeParameters)
    , "super" .= (c ^. classSuper)
    , "interfaces" .= (c ^. classInterfaces)
    , "fields" .= (c ^. classFields)
    , "methods" .= (c ^. classMethods)
    , "bootstrap_methods" .= (c ^. classBootstrapMethods)
    , "enclosing" .= case c ^. classEnclosingMethod of
        Just (cls,mm) ->  Object ("class" .= cls <> "method" .= mm)
        Nothing -> Null
    , "inner_classes" .= (c ^. classInnerClasses)
    , "annotations" .= (c ^. classAnnotations)
    , "version" .= (c ^. classVersion)
    ]

instance ToJSON Field 

instance ToJSON Method 

instance ToJSON BootstrapMethod 

instance ToJSON ReturnType 
instance ToJSON Parameter 
instance ToJSON PAccessFlag 

instance ToJSON TypeParameter 
instance ToJSON ThrowsType 
instance ToJSON TypeVariableName 
instance ToJSON TypeVariable 
instance ToJSON ClassType
instance ToJSON TypeArgument
instance ToJSON ReferenceType
instance ToJSON ArrayType
instance ToJSON Type
instance ToJSON JBaseType
instance ToJSON InnerClass

instance ToJSON Annotation
instance ToJSON AnnotationValue
instance ToJSON (EnumValue JVM.High)

