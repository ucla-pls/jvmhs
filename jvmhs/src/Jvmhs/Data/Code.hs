{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Jvmhs.Data.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module works with the Code. This is a work in progress.
-}

module Jvmhs.Data.Code
  ( Code (..)
  , codeMaxStack
  , codeMaxLocals
  , codeByteCode
  , codeExceptionTable
  , codeStackMap

  , traverseCode

  , fromBinaryCode
  , toBinaryCode

  , ExceptionHandler (..)
  , ehStart
  , ehEnd
  , ehHandler
  , ehCatchType

  , traverseExceptionHandler

  , StackMapTable
  , verificationTypeInfo
  , VerificationTypeInfo

  , ByteCodeOpr
  )
  where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Word
import           GHC.Generics (Generic)
import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.Code          as B
import qualified Language.JVM.Attribute.StackMapTable as B

import           Jvmhs.Data.Type

type ByteCodeOpr = B.ByteCodeOpr B.High
-- type LineNumberTable = B.LineNumberTable B.High
type StackMapTable = B.StackMapTable B.High
type VerificationTypeInfo = B.VerificationTypeInfo B.High

data Code = Code
  { _codeMaxStack       :: !Word16
  , _codeMaxLocals      :: !Word16
  , _codeByteCode       :: ![ByteCodeOpr]
  , _codeExceptionTable :: ![ExceptionHandler]
  , _codeStackMap       :: !(Maybe StackMapTable)
  } deriving (Show, Eq, Generic, NFData)

data ExceptionHandler = ExceptionHandler
  { _ehStart     :: !Word16
  , _ehEnd       :: !Word16
  , _ehHandler   :: !Word16
  , _ehCatchType :: !(Maybe ClassName)
  } deriving (Show, Eq, Generic, NFData)

makeLenses ''Code
makeLenses ''ExceptionHandler

fromBinaryCode :: B.Code B.High -> Code
fromBinaryCode =
  Code
    <$> B.codeMaxStack
    <*> B.codeMaxLocals
    <*> B.codeByteCodeOprs
    <*> fmap fromBinaryExceptionTable . B.unSizedList . B.codeExceptionTable
    <*> B.codeStackMapTable

toBinaryCode :: Code -> B.Code B.High
toBinaryCode c =
  B.Code
   (c^.codeMaxStack)
   (c^.codeMaxLocals)
   (B.ByteCode $ c^.codeByteCode)
   (B.SizedList $ c^..codeExceptionTable.folded.to toBinaryExceptionTable)
   (B.CodeAttributes (maybe [] (:[]) $ c^.codeStackMap) [] [])

fromBinaryExceptionTable :: B.ExceptionTable B.High -> ExceptionHandler
fromBinaryExceptionTable =
  ExceptionHandler
  <$> B.start
  <*> B.end
  <*> B.handler
  <*> B.value . B.catchType

toBinaryExceptionTable :: ExceptionHandler -> B.ExceptionTable B.High
toBinaryExceptionTable =
  B.ExceptionTable
  <$> _ehStart
  <*> _ehEnd
  <*> _ehHandler
  <*> B.RefV . _ehCatchType

traverseCode ::
     (Traversal' Word16 a)
  -> (Traversal' Word16 a)
  -> (Traversal' [ByteCodeOpr] a)
  -> (Traversal' [ExceptionHandler] a)
  -> (Traversal' (Maybe StackMapTable) a)
  -> Traversal' Code a
traverseCode tms tml tbc tet tst g (Code ms ml bc et st) =
    Code
    <$> tms g ms
    <*> tml g ml
    <*> tbc g bc
    <*> tet g et
    <*> tst g st
{-# INLINE traverseCode #-}

traverseExceptionHandler ::
     Traversal' Word16 a
  -> Traversal' Word16 a
  -> Traversal' Word16 a
  -> Traversal' (Maybe ClassName) a
  -> Traversal' ExceptionHandler a
traverseExceptionHandler ts te th tc g (ExceptionHandler s e h c) =
  ExceptionHandler <$> ts g s <*> te g e <*> th g h <*> tc g c
{-# INLINE traverseExceptionHandler #-}

verificationTypeInfo :: Traversal' StackMapTable VerificationTypeInfo
verificationTypeInfo g (B.StackMapTable s) =
  B.StackMapTable <$> (traverse . ver) g s
  where
    ver f (B.StackMapFrame fs ft) =
      B.StackMapFrame fs <$>
      case ft of
        B.SameLocals1StackItemFrame v -> B.SameLocals1StackItemFrame <$> f v
        B.AppendFrame v -> B.AppendFrame <$> traverse f v
        B.FullFrame v1 v2 -> B.FullFrame <$> traverse f v1 <*> traverse f v2
        _ -> pure ft

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5} ''Code)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ExceptionHandler)

instance ToJSON (B.ByteCodeOpr B.High) where
  toJSON _ = String "<code>"

instance ToJSON (B.StackMapTable B.High) where
  toJSON _ = String "<stack-map-table>"
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
