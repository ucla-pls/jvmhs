{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.HierarchyTest where

-- import SpecHelper
-- import Jvmhs

-- withHierarchy :: SpecWith Hierarchy -> Spec
-- withHierarchy = beforeClassPool $
--   snd <$> getHierarchy

-- spec_implementations :: Spec
-- spec_implementations = withHierarchy $ do
--   it "can find the subclasses of 'java.lang.Object'" $ \hry -> do
--     implementations hry "java.lang.Object" `shouldBe` ["java.lang.Object", "Simple", "Extended"]

--   it "can find that 'Extended' has no subclasses" $ \hry -> do
--     implementations hry "Extended" `shouldBe` ["Extended"]

-- spec_superclasses :: Spec
-- spec_superclasses = withHierarchy $ do
--   it "can find the superclasses of 'Extended'" $ \hry -> do
--     superclasses hry "Extended" `shouldBe` ["Simple", "java.lang.Object"]

--   it "can find that 'java.lang.Object' has no super classes " $ \hry -> do
--     superclasses hry "java.lang.Object" `shouldBe` []

-- spec_fieldFromId :: Spec
-- spec_fieldFromId = do
--   let runClassNameOf = runTestClassPool' . classNameOfFieldId

--   it "can find 'extField' in 'Extended'" $ do
--     x <- runClassNameOf (inClass "Extended" "extField:I")
--     x `shouldBe` Just "Extended"

--   it "should not find 'extField' in 'Simple'" $ do
--     x <- runClassNameOf (inClass "Simple" "extField:I")
--     x `shouldBe` Nothing

--   it "can find 'field' in 'Simple'" $ do
--     x <- runClassNameOf (inClass "Simple" "field:LSimple;")
--     x `shouldBe` Just "Simple"


-- spec_methodFromId :: Spec
-- spec_methodFromId = do
--   let runClassNameOf = runTestClassPool' . classNameOfMethodId

--   it "can find 'method1' in 'Extended'" $ do
--     x <- runClassNameOf (inClass "Extended" "method1:()V")
--     x `shouldBe` Just "Extended"

--   it "can handle inheritance" $ do
--     x <- runClassNameOf (inClass "Extended" "method2:()V")
--     x `shouldBe` Just "Simple"

--   it "can handle overriding methods" $ do
--     x <- runClassNameOf (inClass "Extended" "method3:(I)I")
--     x `shouldBe` Just "Extended"

--   it "will not find unknown methods" $ do
--     x <- runClassNameOf (inClass "Extended" "undefined:()V")
--     x `shouldBe` Nothing

-- spec_methodImpls :: Spec
-- spec_methodImpls = withHierarchy $ do
--   let runMethodImpls hry = runTestClassPool' . methodImpls hry
--   it "finds two implementations of method3:(I)I" $ \hry -> do
--     x <- runMethodImpls hry (inClass "Simple" "method3:(I)I")
--     x `shouldSatisfy` (== 2) . length

--   it "finds two implementations from interface" $ \hry -> do
--     x <- runMethodImpls hry (inClass "Interface" "method4:()V")
--     x `shouldSatisfy` (== 1) . length
