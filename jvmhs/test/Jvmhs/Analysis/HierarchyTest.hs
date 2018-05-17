{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.HierarchyTest where

import SpecHelper
import Jvmhs

withHierarchy :: SpecWith Hierarchy -> Spec
withHierarchy = beforeClassPool $
  calculateHierarchy ["Extended", "Simple"]

spec_implementations :: SpecWith ()
spec_implementations = withHierarchy $ do
  it "can find the subclasses of 'java.lang.Object'" $ \hry -> do
    implementations hry "java.lang.Object" `shouldBe` ["Simple", "Extended"]

  it "can find that 'Extended' has no subclasses" $ \hry -> do
    implementations hry "Extended" `shouldBe` []

spec_superclasses :: SpecWith ()
spec_superclasses = withHierarchy $ do
  it "can find the superclasses of 'Extended'" $ \hry -> do
    superclasses hry "Extended" `shouldBe` ["Simple", "java.lang.Object"]

  it "can find that 'java.lang.Object' has no super classes " $ \hry -> do
    superclasses hry "java.lang.Object" `shouldBe` []


spec_fieldFromId :: Spec
spec_fieldFromId = do
  it "can find 'extField' in 'Extended'" $ do
    x <- runTestClassPool' $ fieldFromId "extField:I" "Extended"
    (fst <$> x) `shouldBe` Just "Extended"

  it "should not find 'extField' in 'Simple'" $ do
    x <- runTestClassPool' $ fieldFromId "extField:I" "Simple"
    (fst <$> x) `shouldBe` Nothing

  it "can find 'field' in 'Simple'" $ do
    x <- runTestClassPool' $ fieldFromId "field:LSimple;" "Extended"
    (fst <$> x) `shouldBe` Just "Simple"
