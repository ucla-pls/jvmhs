module SpecHelper
  ( module Test.Tasty.Hspec
  , module Test.Tasty.QuickCheck
  , module Control.Lens
  , decode
  , encode
  , blReadFile
  , isoBinary
  ) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as BL

import Data.Binary
import Data.Bits
import Control.Lens hiding (elements)

blReadFile :: FilePath -> IO BL.ByteString
blReadFile = BL.readFile

toHex :: Word8 -> String
toHex x =
  [ alpha !! fromIntegral (x `shift` (-4))
  , alpha !! fromIntegral (x `mod` 16)
  ]
  where alpha = "0123456789abcdef"

isoBinary :: (Binary a, Eq a, Show a) => a -> Property
isoBinary a =
  let bs = encode a
  in counterexample (concat . map toHex $ BL.unpack bs) $
      decode bs === a
