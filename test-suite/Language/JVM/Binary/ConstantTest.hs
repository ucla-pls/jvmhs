{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Binary.ConstantTest where

import SpecHelper

import qualified Data.ByteString as BS

import Language.JVM.Binary.Constant

import qualified Data.IntMap as IM

prop_encode_and_decode :: ConstantPool -> Bool
prop_encode_and_decode attr =
  (decode . encode) attr == attr


instance Arbitrary ConstantRef where
  arbitrary =
    ConstantRef <$> arbitrary

instance Arbitrary ConstantPool where
  arbitrary =
    ConstantPool . IM.fromList . go 1 <$> arbitrary
    where
      go n (e : lst) =
        (n, e) : go (n + poolSize e) lst
      go _ [] = []


instance Arbitrary Constant where
  arbitrary = oneof
    [ String <$> binstr
    , Integer <$> arbitrary
    , Float <$> arbitrary
    , Long <$> arbitrary
    , Double <$> arbitrary
    , ClassRef <$> arbitrary
    , StringRef <$> arbitrary
    , FieldRef <$> arbitrary <*> arbitrary
    , MethodRef <$> arbitrary <*> arbitrary
    , InterfaceMethodRef <$> arbitrary <*> arbitrary
    , NameAndType <$> arbitrary <*> arbitrary
    , MethodHandle <$> arbitrary <*> arbitrary
    , MethodType <$> arbitrary
    , InvokeDynamic <$> arbitrary <*> arbitrary
    ]

binstr :: Gen BS.ByteString
binstr = do
  len <- choose (0, 50)
  BS.pack <$> sequence (replicate len arbitrary)
