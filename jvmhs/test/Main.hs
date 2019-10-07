module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec

main :: IO ()
main = hspecWith defaultConfig
  { configFastFail = True
  , configFormatter = Just progress
  , configFailureReport = Just "failures.txt"
  , configRerun = True
  } Spec.spec
