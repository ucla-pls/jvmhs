{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : Jvmhs.Data.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module works with the Code. This is a work in progress.
-}

module Jvmhs.Data.Code
  where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import           Data.Word
import           GHC.Generics
import qualified Language.JVM                as B
import qualified Language.JVM.Attribute.Code as B
import qualified Language.JVM.Attribute.StackMapTable as B

import Jvmhs.Data.Type

newtype Code = Code
  { _unCode :: B.Code B.High
  } deriving (Show, Eq, Generic, NFData)

makeLenses ''Code

makeLens ::
  (B.Code B.High -> a)
  -> (B.Code B.High -> a -> B.Code B.High)
  -> Lens' Code a
makeLens sa sbt  = unCode . lens sa sbt

codeMaxStack :: Lens' Code Word16
codeMaxStack =
  makeLens B.codeMaxStack (\s b -> s { B.codeMaxStack = b })

codeMaxLocals :: Lens' Code Word16
codeMaxLocals =
  makeLens B.codeMaxLocals (\s b -> s { B.codeMaxLocals = b })

codeByteCode :: Lens' Code [ByteCodeOpr]
codeByteCode =
  makeLens
    (B.unByteCode . B.codeByteCode)
    (\s b -> s { B.codeByteCode = B.ByteCode b })

codeExceptionTables :: Lens' Code [ExceptionTable]
codeExceptionTables =
  makeLens
    (B.unSizedList . B.codeExceptionTable)
    (\s b -> s { B.codeExceptionTable = B.SizedList b })

codeAttributes :: Lens' Code CodeAttributes
codeAttributes =
  makeLens
    (B.codeAttributes)
    (\s b -> s { B.codeAttributes = b })

traverseCode ::
     (Traversal' Word16 a)
  -> (Traversal' Word16 a)
  -> (Traversal' [ByteCodeOpr] a)
  -> (Traversal' [ExceptionTable] a)
  -> (Traversal' CodeAttributes a)
  -> Traversal' Code a
  -- Applicative f => (a -> f a) -> Code -> f Code
traverseCode tms tml tbc tet ta g =
  fmap Code . tC . _unCode
  where
    tC (B.Code ms ml bc et a) =
      B.Code
      <$> tms g ms
      <*> tml g ml
      <*> (unByteCode . tbc) g bc
      <*> (unSizedList . tet) g et
      <*> ta g a

    unByteCode f = fmap B.ByteCode . f . B.unByteCode
    unSizedList f = fmap B.SizedList . f . B.unSizedList

{-# INLINE traverseCode #-}

type CodeAttributes = B.CodeAttributes B.High

traverseCodeAttributes ::
      Traversal' [ StackMapTable ] a
   -> Traversal' [ LineNumberTable ] a
   -> Traversal' [ B.Attribute B.High ] a
   -> Traversal' CodeAttributes a
traverseCodeAttributes tsm tln tas f (B.CodeAttributes sm ln as) =
  B.CodeAttributes <$> tsm f sm <*> tln f ln <*> tas f as
{-# INLINE traverseCodeAttributes #-}

type ByteCodeOpr = B.ByteCodeOpr B.High
type LineNumberTable = B.LineNumberTable B.High

type StackMapTable = B.StackMapTable B.High
type VerificationTypeInfo = B.VerificationTypeInfo B.High

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

type ExceptionTable = B.ExceptionTable B.High
exceptionCatchType :: Lens' ExceptionTable (Maybe ClassName)
exceptionCatchType =
  lens (B.value.B.catchType) (\s b -> s { B.catchType = B.RefV b})

instance ToJSON Code where
    toJSON _ = String $ "<code>"
