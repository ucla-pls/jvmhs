{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Binary.FieldTest where

import SpecHelper

import Language.JVM.Binary.Field
import Language.JVM.Binary.SizedListTest ()
import Language.JVM.Binary.ConstantTest ()
import Language.JVM.Binary.AttributeTest ()

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
