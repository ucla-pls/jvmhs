{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Jvmhs.Data.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module works with the Code. This is a work in progress.
-}
module Jvmhs.Data.Code (
  Code (..),
  codeMaxStack,
  codeMaxLocals,
  codeByteCode,
  codeExceptionTable,
  codeStackMap,
  codeLineNumbers,
  codeAnnotations,
  ExceptionHandler (..),
  ehStart,
  ehEnd,
  ehHandler,
  ehCatchType,
  verificationTypeInfo,

  -- * ByteCodeInst
  ByteCodeInst,
  byteCodeOffset,
  byteCodeOpcode,

  -- * TypeName
  TypeName (..),
  fromSmallArithmeticType,
  fromArithmeticType,
  fromArrayType,
  fromLocalType,

  -- * CodeTypeAnnotations
  CodeAnnotation (..),
  ctTarget,
  ctPath,
  ctAnnotation,

  -- * Diagrams
  Data.Cone.Diagram (..),

  -- * Re-exports
  B.StackMapTable (..),
  B.StackMapFrame (..),
  B.StackMapFrameType (..),
  B.VerificationTypeInfo (..),
  B.CodeTypeAnnotation (..),
  B.LocalVarEntry (..),
  B.LocalVarTarget,
  B.TypeArgumentTarget (..),
  B.CatchTarget,
  B.OffsetTarget,
) where

-- base
import Data.Word
import GHC.Generics (Generic)

-- nfdata
import Control.DeepSeq

-- lens
import Control.Lens hiding ((.=))

-- text
import qualified Data.Vector as V

-- jvm-binary
import qualified Language.JVM as B
import qualified Language.JVM.Attribute.StackMapTable as B

-- cones
import qualified Data.Cone
import Data.Cone.TH

import Data.IntMap (IntMap)
import Jvmhs.Data.Identifier
import Jvmhs.Data.Type (Annotation, TypePath)
import qualified Language.JVM.Attribute.Annotations as B
import Language.JVM.Attribute.LineNumberTable (LineNumber)

type ByteCodeInst = B.ByteCodeInst B.High

type StackMapTable = B.StackMapTable B.High
type LineNumberTable = IntMap LineNumber
type VerificationTypeInfo = B.VerificationTypeInfo B.High

data Code = Code
  { _codeMaxStack :: !Word16
  , _codeMaxLocals :: !Word16
  , _codeByteCode :: !(V.Vector ByteCodeInst)
  , _codeExceptionTable :: ![ExceptionHandler]
  , _codeStackMap :: !(Maybe StackMapTable)
  , _codeLineNumbers :: !(Maybe LineNumberTable)
  , _codeAnnotations :: ![CodeAnnotation]
  }
  deriving (Show, Eq, Generic, NFData)

data CodeAnnotation = CodeAnnotation
  { _ctTarget :: !(B.CodeTypeAnnotation B.High)
  , _ctPath :: !TypePath
  , _ctAnnotation :: !Annotation
  }
  deriving (Eq, Show, Generic, NFData)

data ExceptionHandler = ExceptionHandler
  { _ehStart :: !Int
  , _ehEnd :: !Int
  , _ehHandler :: !Int
  , _ehCatchType :: !(Maybe ClassName)
  }
  deriving (Show, Eq, Generic, NFData)

makeLenses ''Code
makeLenses ''CodeAnnotation
makeLenses ''ExceptionHandler

byteCodeOffset :: Lens' ByteCodeInst B.ByteCodeOffset
byteCodeOffset = lens B.offset (\a b -> a{B.offset = b})

byteCodeOpcode :: Lens' ByteCodeInst (B.ByteCodeOpr B.High)
byteCodeOpcode = lens B.opcode (\a b -> a{B.opcode = b})

verificationTypeInfo :: Traversal' StackMapTable VerificationTypeInfo
verificationTypeInfo g (B.StackMapTable s) =
  B.StackMapTable <$> (traverse . ver) g s
 where
  ver f (B.StackMapFrame fs ft) =
    B.StackMapFrame fs <$> case ft of
      B.SameLocals1StackItemFrame v -> B.SameLocals1StackItemFrame <$> f v
      B.AppendFrame v -> B.AppendFrame <$> traverse f v
      B.FullFrame v1 v2 -> B.FullFrame <$> traverse f v1 <*> traverse f v2
      _ -> pure ft

-- * Type Conversion

data TypeName
  = STInteger
  | STLong
  | STShort
  | STChar
  | STByte
  | STBoolean
  | STFloat
  | STDouble
  | STRef
  deriving (Show, Eq, Enum)

fromSmallArithmeticType :: B.SmallArithmeticType -> TypeName
fromSmallArithmeticType = \case
  B.MByte -> STByte
  B.MChar -> STChar
  B.MShort -> STShort

fromArithmeticType :: B.ArithmeticType -> TypeName
fromArithmeticType = \case
  B.MInt -> STInteger
  B.MLong -> STLong
  B.MFloat -> STFloat
  B.MDouble -> STDouble

fromArrayType :: B.ArrayType -> TypeName
fromArrayType = \case
  B.AByte -> STByte
  B.AChar -> STChar
  B.AShort -> STShort
  B.AInt -> STInteger
  B.ALong -> STLong
  B.AFloat -> STFloat
  B.ADouble -> STDouble
  B.ARef -> STRef

fromLocalType :: B.LocalType -> TypeName
fromLocalType = \case
  B.LInt -> STInteger
  B.LLong -> STLong
  B.LFloat -> STFloat
  B.LDouble -> STDouble
  B.LRef -> STRef

$(makeDiagram ''Code)
$(makeDiagram ''ExceptionHandler)
$(makeDiagram ''CodeAnnotation)
$(makeDiagram ''B.ByteCodeInst)
$(makeDiagram ''B.StackMapFrameType)

$(makeDiagramLite ''VerificationTypeInfo)
$(makeDiagram ''TypeName)

type ByteCodeOpr = B.ByteCodeOpr B.High
$(makeDiagramLite ''ByteCodeOpr)

$(makeDiagram ''B.BinOpr)
$(makeDiagram ''B.OneOrTwo)
$(makeDiagram ''B.BitOpr)
$(makeDiagram ''B.CastOpr)
$(makeDiagram ''B.CmpOpr)

type SwitchTable = B.SwitchTable B.High
$(makeDiagramLite ''SwitchTable)

type Invocation = B.Invocation B.High
$(makeDiagramLite ''Invocation)

type InvokeDynamic = B.InvokeDynamic B.High
$(makeDiagramLite ''InvokeDynamic)

type StackMapFrame = B.StackMapFrame B.High
$(makeDiagramLite ''StackMapFrame)

type TypeArgumentTarget = B.TypeArgumentTarget B.High
$(makeDiagramLite ''TypeArgumentTarget)

type CodeTypeAnnotation = B.CodeTypeAnnotation B.High
$(makeDiagramLite ''CodeTypeAnnotation)

type LocalVarEntry = B.LocalVarEntry B.High
$(makeDiagramLite ''LocalVarEntry)
