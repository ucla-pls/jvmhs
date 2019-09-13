{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Jvmhs.Transform.Stub
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module describs a stubbing transformation that given a method, returns a
new stubbed method.

-}

module Jvmhs.Transform.Stub where

-- base
import Data.Word

-- lens
import Control.Lens

-- vector
import qualified Data.Vector as V

import qualified Language.JVM.ByteCode as B
import qualified Language.JVM.Constant as B

-- jvmhs
import Jvmhs.Data.Class
import Jvmhs.Data.Type

-- | If the method has a code and it is not a constructor then stub it.
stub :: Method -> Method
stub m
  | isConstructor m = m
  | otherwise =
    m & methodCode . _Just .~ makeStub requiredLocals (m ^. methodReturnType)
  where
    requiredLocals =
      (if not $ m ^. methodAccessFlags . contains MStatic then (+ 1) else id)
      (sumOf (methodArgumentTypes.folded.to typeSize) m)

-- | Create a new code instance with the given return type.
makeStub :: Word16 -> Maybe JType -> Code
makeStub requiredLocals jt = Code
  { _codeMaxStack = maybe 1 typeSize jt
  , _codeMaxLocals = requiredLocals
  , _codeByteCode = V.fromList . map (B.ByteCodeInst 0) $ case jt of
      Nothing -> [ B.Return Nothing ]
      Just (JTBase a) -> case a of
        JTInt     -> return0Int
        JTShort   -> return0Int
        JTByte    -> return0Int
        JTChar    -> return0Int
        JTBoolean -> return0Int
        JTLong    -> [ B.Push (Just (B.VLong 0)), B.Return (Just B.LLong)]
        JTFloat   -> [ B.Push (Just (B.VFloat 0)), B.Return (Just B.LFloat)]
        JTDouble  -> [ B.Push (Just (B.VDouble 0)), B.Return (Just B.LDouble)]
      Just (JTRef _) -> [ B.Push Nothing, B.Return (Just B.LRef)]
  , _codeExceptionTable = []
  , _codeStackMap = Nothing
  }
  where return0Int = [ B.Push (Just (B.VInteger 0)), B.Return (Just B.LInt)]

-- | A Type in Java have different sizes.
typeSize :: JType -> Word16
typeSize = \case
  JTBase JTLong -> 2
  JTBase JTDouble -> 2
  _ -> 1

isConstructor :: Method -> Bool
isConstructor m = m^.methodId == "<init>"
