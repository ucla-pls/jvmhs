{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Binary.AttributeTest where

import SpecHelper

import Language.JVM.Binary.Attribute (Attribute (..))
import Language.JVM.Binary.ConstantTest ()

import qualified Data.ByteString as BS

prop_encode_and_decode :: Attribute -> Bool
prop_encode_and_decode attr =
  (decode . encode) attr == (attr :: Attribute)

instance Arbitrary Attribute where
  arbitrary = do
    index <- arbitrary
    len <- choose (0, 50)
    bs <- BS.pack <$> sequence (replicate len arbitrary)
    return $ Attribute index bs
