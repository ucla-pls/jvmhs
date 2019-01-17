{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
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

-- base
import Control.Monad

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
  { stackLocals :: V.Vector TypeInfo
  , stackStack :: [TypeInfo]
  } deriving (Show, Eq)

mkStack :: [TypeInfo] -> [TypeInfo] -> Stack
mkStack l s = Stack (V.fromList l) s

checkMethod :: Method -> Maybe (V.Vector Stack)
checkMethod (Method (Named n cnt)) =
  case _methodCode cnt of
    Just code ->
      let types = n ^. methodArgumentTypes
      in checkCode (Stack (V.fromList $ map typeInfoFromJType types) []) code
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

checkCode :: Stack -> Code -> Maybe (V.Vector Stack)
checkCode stack code =
  checkByteCodeOprs stack (_codeByteCode code)

reduceStackMap :: V.Vector Stack -> Maybe StackMapTable
reduceStackMap _ = Nothing
  -- B.StackMapTable (map toFullFrame . V.toList $ v)
  -- where
  --   toFullFrame (Stack l s) =
  --     B.StackMapFrame 1
  --     $ B.FullFrame (B.SizedList (V.toList l)) (B.SizedList s)

checkByteCodeOprs :: Stack -> V.Vector ByteCodeOpr -> Maybe (V.Vector Stack)
checkByteCodeOprs s v = do
  V.fromList <$> puttogetter checkByteCodeOpr s (V.toList v)
  where
    puttogetter :: (b -> a -> Maybe a) -> a -> [b] -> Maybe [a]
    puttogetter fn a ls =
      case ls of
        [] -> Just []
        b:rest -> do
          x <- fn b a
          rst <- puttogetter fn x rest
          return (x:rst)

checkByteCodeOpr :: ByteCodeOpr -> Stack -> Maybe Stack
checkByteCodeOpr opr stack =
  case opr of
    B.Nop -> Just stack
    B.Push c -> justPush $ maybe B.VTNull typeInfoOf c
    B.Load lt idx -> do
      t <- stackLocals stack V.!? fromIntegral idx
      guard (isLocalTypeEq lt t)
      justPush t

    B.Return mlt ->
      case mlt of
        Just lt ->
          popIf (isLocalTypeEq lt)
        Nothing ->
          Just stack
    _ -> Nothing

  where
    justPush i = Just $ push i stack

    popIf fn =
      case stackStack stack of
        l:rest
          | fn l ->
              Just (stack { stackStack = rest })
        _ -> Nothing

push :: TypeInfo -> Stack -> Stack
push i stack = stack { stackStack = i:(stackStack stack)}

typeInfoOf :: B.JValue -> TypeInfo
typeInfoOf = \case
  B.VInteger _ -> B.VTInteger
  B.VLong _ -> B.VTLong
  B.VFloat _ ->B.VTFloat
  B.VDouble _ -> B.VTDouble
  B.VString _ -> B.VTObject "java/lang/String"
  B.VClass _ -> B.VTObject "java/lang/Class"
  B.VMethodType _ -> B.VTObject "java/lang/invoke/MethodType"
  B.VMethodHandle _ -> B.VTObject "java/lang/invoke/MethodHandle"

isLocalTypeEq :: B.LocalType -> TypeInfo -> Bool
isLocalTypeEq a b =
  case (a, b) of
    (B.LInt, B.VTInteger) -> True
    _ -> False
