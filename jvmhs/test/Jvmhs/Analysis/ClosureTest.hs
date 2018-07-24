{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.ClosureTest where

import qualified Data.Set as S

import SpecHelper
import Jvmhs

withHierarchy :: SpecWith Hierarchy -> Spec
withHierarchy = beforeClassPool $
  calculateHierarchy =<< allClassNames

spec_methodClosure :: Spec
spec_methodClosure = do
  it "can find the methods of 'Extended'" $ do
    ms <- runTestClassPool' $ do
      hry <- calculateHierarchy =<< allClassNames
      computeMethodClosure hry (S.fromList [inClass "Extended" "method1:()V"])
    S.toList ms
      `shouldBe`
      [inClass "Extended" "method1:()V", inClass "Simple" "method2:()V"]
