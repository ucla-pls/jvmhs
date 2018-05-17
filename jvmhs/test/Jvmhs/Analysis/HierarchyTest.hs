{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.HierarchyTest where

import SpecHelper
import Jvmhs

withHierarchy :: SpecWith Hierarchy -> Spec
withHierarchy = beforeClassPool $
  calculateHierarchy ["Extended", "Simple"]

spec_implementations :: Spec
spec_implementations = withHierarchy $ do
  it "can find the subclasses of 'java.lang.Object'" $ \hry -> do
    implementations hry "java.lang.Object" `shouldBe` ["Simple", "Extended"]

  it "can find that 'Extended' has no subclasses" $ \hry -> do
    implementations hry "Extended" `shouldBe` []

spec_superclasses :: Spec
spec_superclasses = withHierarchy $ do
  it "can find the superclasses of 'Extended'" $ \hry -> do
    superclasses hry "Extended" `shouldBe` ["Simple", "java.lang.Object"]

  it "can find that 'java.lang.Object' has no super classes " $ \hry -> do
    superclasses hry "java.lang.Object" `shouldBe` []

spec_fieldFromId :: Spec
spec_fieldFromId = do
  let runClassNameOf fid cl = runTestClassPool' $ classNameOfFieldId fid cl

  it "can find 'extField' in 'Extended'" $ do
    x <- runClassNameOf "extField:I" "Extended"
    x `shouldBe` Just "Extended"

  it "should not find 'extField' in 'Simple'" $ do
    x <- runClassNameOf "extField:I" "Simple"
    x `shouldBe` Nothing

  it "can find 'field' in 'Simple'" $ do
    x <- runClassNameOf "field:LSimple;"  "Simple"
    x `shouldBe` Just "Simple"


spec_methodFromId :: Spec
spec_methodFromId = do
  let runClassNameOf mid cl = runTestClassPool' $ classNameOfMethodId mid cl

  it "can find 'method1' in 'Extended'" $ do
    x <- runClassNameOf "method1:()V" "Extended"
    x `shouldBe` Just "Extended"

  it "can handle inheritance" $ do
    x <- runClassNameOf "method2:()V" "Extended"
    x `shouldBe` Just "Simple"

  it "can handle overriding methods" $ do
    x <- runClassNameOf "method3:(I)I" "Extended"
    x `shouldBe` Just "Extended"

  it "will not find unknown methods" $ do
    x <- runClassNameOf "undefined:()V" "Extended"
    x `shouldBe` Nothing
