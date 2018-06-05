{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.ReduceTest where

import SpecHelper
import Jvmhs
import Jvmhs.Analysis.Reduce

import qualified Data.Set as S
import qualified Data.Map as M

--outputPath :: FilePath
--outputPath = "test/output/interface"
--

spec_unusedInterfaces :: Spec
spec_unusedInterfaces =
  it "should find (unused interfaces, their parents) pairs" $ do
    x <- runTestClassPool' $ do
      -- (found, _) <- computeClassClosure (S.singleton  ("SimpleI"))
      iMap  <- findUnusedInterfaces
      return $ M.toList iMap
    x `shouldContain` [("Itfc2", S.fromList ["ItfcParent"])]


prop_makeConsistent :: Property
prop_makeConsistent =
  forAll genIFMappings $ \ m ->
    let x = toCannoicalIFMapping m in
    S.unions (M.elems x) `S.intersection` M.keysSet x === S.empty


genIFMappings :: Gen IFMapping
genIFMappings =
  M.fromList <$> listOf genIFPair
  where
    genIFPair = do
     cn <- elements testClassNames
     parrents <- S.fromList <$> sublistOf (filter (> cn) testClassNames)
     return (cn, parrents)

    testClassNames =
      ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","U","V","W","X","Y","Z"]

spec_inlineReplaceMap :: Spec
spec_inlineReplaceMap = do
  let unusedMap = (M.fromList
        [ ("Itfc1", S.fromList ["ItfcParent1", "ItfcParent2"])
        , ("Itfc2", S.fromList  ["ItfcParent4"])
        , ("Itfc3", S.fromList  [])
        , ("ItfcParent2", S.fromList  ["ItfcParent3"])
        , ("ItfcParent3", S.fromList  ["ItfcParent4"])
        ]) :: M.Map ClassName (S.Set ClassName)

  let mapAfterReplacement = (M.fromList
        [ ("Itfc1", S.fromList ["ItfcParent1", "ItfcParent4"])
        , ("Itfc2", S.fromList ["ItfcParent4"])
        , ("Itfc3", S.fromList [])
        , ("ItfcParent2", S.fromList ["ItfcParent4"])
        , ("ItfcParent3", S.fromList  ["ItfcParent4"])
        ]) :: M.Map ClassName (S.Set ClassName)
  it "should resursively replace val if vals are contain in the keySet" $ do
    (M.toList $ toCannoicalIFMapping unusedMap)
      `shouldMatchList` (M.toList mapAfterReplacement)

spec_inlineInterfaces :: Spec
spec_inlineInterfaces =
  it "should replace unused interfaces with its parents recursively" $ do
    x <- runTestClassPool' $ do
      -- (found, _) <- computeClassClosure (S.singleton  ("SimpleI"))

      iMap  <- findUnusedInterfaces
      Just cls <- getClass "SimpleI"
      let newCls = inlineInterfaces iMap cls
      return $ newCls ^. classInterfaces
    x `shouldMatchList` ["Itfc", "ItfcParent"]

spec_reduceInterfaces :: Spec
spec_reduceInterfaces = do
  it "should not change the interfaces when not run" $ do
    Just x <- runTestClassPool' $ do
      getClass "SimpleI"
    (x^.classInterfaces) `shouldMatchList` ["Itfc2", "Itfc"]
  it "should remove an interface" $ do
    Just x <- runTestClassPool' $ do
      reduceInterfaces -- ["ItfcParent", "Itfc", "Itfc2", "SimpleI"]
      getClass "SimpleI"
    (x^.classInterfaces) `shouldMatchList` ["ItfcParent", "Itfc"]
