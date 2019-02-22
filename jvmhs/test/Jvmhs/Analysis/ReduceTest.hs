{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Analysis.ReduceTest where

import           Jvmhs
import           Jvmhs.Analysis.Reduce
import           SpecHelper

import Data.Functor

import qualified Data.HashMap.Strict   as M
import qualified Data.HashSet          as S
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
    S.unions (M.elems x) `S.intersection` S.fromMap (x $> ()) === S.empty


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
        ]) :: M.HashMap ClassName (S.HashSet ClassName)

  let mapAfterReplacement = (M.fromList
        [ ("Itfc1", S.fromList ["ItfcParent1", "ItfcParent4"])
        , ("Itfc2", S.fromList ["ItfcParent4"])
        , ("Itfc3", S.fromList [])
        , ("ItfcParent2", S.fromList ["ItfcParent4"])
        , ("ItfcParent3", S.fromList  ["ItfcParent4"])
        ]) :: M.HashMap ClassName (S.HashSet ClassName)
  it "should resursively replace val if vals are contain in the keySet" $ do
    (M.toList $ toCannoicalIFMapping unusedMap)
      `shouldMatchList` (M.toList mapAfterReplacement)

spec_inlineInterfaces :: Spec
spec_inlineInterfaces =
  it "should replace unused interfaces with its parents recursively" $ do
    x <- runTestClassPool' $ do
      -- (found, _) <- computeClassClosure (S.singleton  ("SimpleI"))
      iMap  <- findUnusedInterfaces
      getClass "SimpleI" >>= \case
        Just cls -> do
          let newCls = inlineInterfaces iMap cls
          return $ newCls ^.. classInterfaces . folded
        Nothing -> do
          return []

    x `shouldMatchList` ["Itfc", "ItfcParent"]

spec_reduceInterfaces :: Spec
spec_reduceInterfaces = do
  it "should not change the interfaces when not run" $ do
    Just x <- runTestClassPool' $ do
      getClass "SimpleI"
    (x^..classInterfaces.folded) `shouldMatchList` ["Itfc2", "Itfc"]
  it "should remove an interface" $ do
    Just x <- runTestClassPool' $ do
      reduceInterfaces -- ["ItfcParent", "Itfc", "Itfc2", "SimpleI"]
      getClass "SimpleI"
    (x^..classInterfaces.folded) `shouldMatchList` ["ItfcParent", "Itfc"]
