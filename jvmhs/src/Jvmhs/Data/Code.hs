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
    tC s =
      B.Code
      <$> (tms g . B.codeMaxStack $ s)
      <*> (tml g . B.codeMaxLocals $ s)
      <*> (fmap B.ByteCode . tbc g . B.unByteCode . B.codeByteCode $ s)
      <*> (fmap B.SizedList . tet g . B.unSizedList . B.codeExceptionTable $ s)
      <*> (ta g . B.codeAttributes $ s)

type ByteCodeOpr = B.ByteCodeOpr B.High
type ExceptionTable = B.ExceptionTable B.High
type LineNumberTable = B.LineNumberTable B.High
type CodeAttributes = B.CodeAttributes B.High

exceptionCatchType :: Lens' ExceptionTable (Maybe ClassName)
exceptionCatchType =
  lens (B.value.B.catchType) (\s b -> s { B.catchType = B.RefV b})

instance ToJSON Code where
    toJSON _ = String $ "<code>"
