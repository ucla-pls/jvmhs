{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.Binary.CodeTest where

import           SpecHelper

import           Control.Monad                     (forM_)
import           Data.Either
import           Data.Word

import           Language.JVM.Binary.Attribute     (name)
import           Language.JVM.Binary.AttributeTest ()
import qualified Language.JVM.Binary.ClassFile     as CF
import           Language.JVM.Binary.Code
import qualified Language.JVM.Binary.Constant      as Constant
import qualified Language.JVM.Binary.Method        as Method
import           Language.JVM.Binary.SizedList
import           Language.JVM.Binary.SizedListTest ()


prop_encode_and_decode_ByteCode :: ByteCode -> Property
prop_encode_and_decode_ByteCode = isoBinary

prop_encode_and_decode :: Code -> Property
prop_encode_and_decode = isoBinary

spec_reading_real_classfile :: Spec
spec_reading_real_classfile = do
  beforeAll (blReadFile "test-suite/project/Main.class") $ do
    it "can read the bytestring" $ \bs ->
      let classfile = decode bs
          cp = CF.constantPool classfile
          ms = unSizedList16 . CF.methods $ classfile
          cs =
            filter (\a -> a ^. name cp == Just "Code")
            . concatMap (unSizedList16 . Method.attributes)
            $ ms
      in forM_ cs (\c -> fromAttribute c `shouldSatisfy` isRight)

instance Arbitrary Code where
  arbitrary = Code
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


instance Arbitrary ArithmeticType where
  arbitrary = elements [ MInt, MLong, MFloat, MDouble ]

instance Arbitrary LocalType where
  arbitrary = elements [ LInt, LLong, LFloat, LDouble, LRef ]

instance Arbitrary ByteCode where
  arbitrary = oneof
    [ pure Nop
    , Push <$> arbitrary
    ]

instance Arbitrary Constant where
  arbitrary = oneof
    [ pure CNull
    , pure CIntM1
    , pure CInt0
    , pure CInt1
    , pure CInt2
    , pure CInt3
    , pure CInt4
    , pure CInt5

    , pure CLong0
    , pure CLong1

    , pure CFloat0
    , pure CFloat1
    , pure CFloat2

    , pure CDouble0
    , pure CDouble1

    , CByte <$> arbitrary
    , CShort <$> arbitrary

    , CHalfRef . Constant.ConstantRef . fromIntegral <$> (arbitrary  :: Gen Word8)
    , CRef One <$> arbitrary
    , CRef Two <$> arbitrary
    ]

instance Arbitrary ByteCodes where
  arbitrary = ByteCodes <$> arbitrary

instance Arbitrary ExceptionTable where
  arbitrary = ExceptionTable
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
