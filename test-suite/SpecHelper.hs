module SpecHelper
  ( module Test.Tasty.Hspec
  , module Test.Tasty.QuickCheck
  , decode
  , encode
  , blReadFile
  ) where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as BL

import Data.Binary

blReadFile :: FilePath -> IO BL.ByteString
blReadFile = BL.readFile
