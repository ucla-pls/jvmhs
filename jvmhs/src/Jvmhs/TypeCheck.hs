{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-
Module      : Jvmhs.TypeCheck
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

-}
module Jvmhs.TypeCheck where

-- lens
import Control.Lens

-- vector
import qualified Data.Vector as V

-- jvm-binary
import qualified Language.JVM.Attribute.StackMapTable as B
import qualified Language.JVM as B

-- jvmhs
import Jvmhs.Data.Class
import Jvmhs.Data.Named
import Jvmhs.Data.Code


type TypeInfo = B.VerificationTypeInfo B.High

data Stack = Stack
  { stackLocals :: [TypeInfo]
  , stackStack :: [TypeInfo]
  } deriving (Show, Eq)

checkMethod :: Method -> Maybe StackMapTable
checkMethod (Method (Named n cnt)) =
  case _methodCode cnt of
    Just code ->
      let types = n ^. methodArgumentTypes
      in checkCode (Stack (map typeInfoFromJType types) []) code
    Nothing ->
      undefined
  where
    typeInfoFromJType :: B.JType -> TypeInfo
    typeInfoFromJType = \case
      B.JTBase b ->
        case b of
          B.JTByte -> B.VTInteger
          B.JTChar -> B.VTInteger
          B.JTDouble -> B.VTDouble
          B.JTFloat -> B.VTFloat
          B.JTShort -> B.VTInteger
          B.JTBoolean -> B.VTInteger
          B.JTInt -> B.VTInteger
          B.JTLong -> B.VTLong
      B.JTClass cn ->
        B.VTObject cn
      B.JTArray _ ->
        undefined

checkCode :: Stack -> Code -> Maybe StackMapTable
checkCode stack code =
  case checkByteCodeOprs stack (_codeByteCode code) of
    Just vs ->
      Just (reduceStackMap vs)
    Nothing ->
      Nothing

reduceStackMap :: V.Vector Stack -> StackMapTable
reduceStackMap _ = B.StackMapTable []

checkByteCodeOprs :: Stack -> V.Vector ByteCodeOpr -> Maybe (V.Vector Stack)
checkByteCodeOprs _ _ =
  undefined

checkByteCodeOpr :: Stack -> ByteCodeOpr -> Maybe Stack
checkByteCodeOpr stack = \case
  B.Nop -> Just stack
  B.Push c ->
    case c of
      Nothing ->
        Just (stack { stackStack = B.VTNull:(stackStack stack)})
      Just c' ->
        case c' of
          B.VInteger _ ->
            Just (stack { stackStack = B.VTInteger:(stackStack stack) })
          _ -> Nothing
  _ -> Nothing
