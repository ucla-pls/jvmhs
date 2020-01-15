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
import           Data.Word

-- lens
import           Control.Lens

-- vector
import qualified Data.Vector                   as V

import qualified Language.JVM.ByteCode         as B
import qualified Language.JVM.Constant         as B

-- jvmhs
import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Identifier
import           Jvmhs.Data.Type

-- | If the method has a code and it is not a constructor then stub it.
stub :: Method -> Method
stub m | isConstructor m = m & methodCode . _Just .~ constructorStub
       | otherwise       = m & methodCode . _Just .~ regularStub
 where
  constructorStub =
    makeConstructorStub "java/lang/Object.<init>:()V" requiredLocals

  regularStub = makeStub requiredLocals (m ^. methodReturnType . simpleType)

  requiredLocals =
    (if not $ m ^. methodAccessFlags . contains MStatic then (+ 1) else id)
      (sumOf (methodIdArgumentTypes . folded . to typeSize) m)

defaultValue :: JType -> Maybe B.JValue
defaultValue = \case
  JTRef  _ -> Nothing
  JTBase a -> Just $ case a of
    JTInt     -> B.VInteger 0
    JTShort   -> B.VInteger 0
    JTByte    -> B.VInteger 0
    JTChar    -> B.VInteger 0
    JTBoolean -> B.VInteger 0
    JTLong    -> B.VLong 0
    JTFloat   -> B.VFloat 0
    JTDouble  -> B.VDouble 0

pushDefaultValueT :: JType -> B.ByteCodeOpr B.High
pushDefaultValueT = B.Push . defaultValue

returnT :: JType -> B.ByteCodeOpr B.High
returnT = B.Return . Just . \case
  JTRef  _ -> B.LRef
  JTBase a -> case a of
    JTInt     -> B.LInt
    JTShort   -> B.LInt
    JTByte    -> B.LInt
    JTChar    -> B.LInt
    JTBoolean -> B.LInt
    JTLong    -> B.LLong
    JTFloat   -> B.LFloat
    JTDouble  -> B.LDouble

-- | Create a new code instance with the given return type.
makeStub :: Word16 -> Maybe JType -> Code
makeStub requiredLocals jt = Code
  { _codeMaxStack       = maybe 1 typeSize jt
  , _codeMaxLocals      = requiredLocals
  , _codeByteCode       = V.fromList . map (B.ByteCodeInst 0) $ case jt of
                            Nothing -> [B.Return Nothing]
                            Just r  -> [pushDefaultValueT r, returnT r]
  , _codeExceptionTable = []
  , _codeStackMap       = Nothing
  }

-- | Create a constructor stub
makeConstructorStub :: AbsMethodId -> Word16 -> Code
makeConstructorStub m requiredLocals = Code
  { _codeMaxStack       = 1 + sum (map typeSize arguments)
  , _codeMaxLocals      = requiredLocals
  , _codeByteCode       = V.fromList
                          .  map (B.ByteCodeInst 0)
                          $  [B.Load B.LRef 0]
                          ++ [ pushDefaultValueT t | t <- arguments ]
                          ++ [ B.Invoke
                               (B.InvkSpecial
                                 (B.AbsVariableMethodId
                                   False
                                   (InRefType (JTClass $ m ^. className)
                                              (m ^. methodId)
                                   )
                                 )
                               )
                             , B.Return Nothing
                             ]
  , _codeExceptionTable = []
  , _codeStackMap       = Nothing
  }
  where arguments = m ^. methodIdArgumentTypes

-- | A Type in Java have different sizes.
typeSize :: JType -> Word16
typeSize = \case
  JTBase JTLong   -> 2
  JTBase JTDouble -> 2
  _               -> 1

isConstructor :: Method -> Bool
isConstructor m = m ^. methodName == "<init>"
