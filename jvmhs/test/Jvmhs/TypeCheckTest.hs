{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.TypeCheckTest where

import           Jvmhs
import           Jvmhs.Data.Code
import           Jvmhs.TypeCheck
import           SpecHelper

import qualified Data.Vector as V
import qualified Language.JVM.Attribute.StackMapTable as B

-- import           Control.Lens


expected_m :: Method -> [ TypeInfo ] -> Maybe [ [ TypeInfo ] ] -> Expectation
expected_m m locals stack = do
  let res = checkMethod m
  res `shouldBe` V.fromList . map (mkStack locals) <$> stack

  case res of
    Just s -> do
      reduceStackMap s `shouldBe` m ^? methodCode . _Just . codeStackMap . _Just
    Nothing ->
      return ()


spec_checkMethod :: Spec
spec_checkMethod = do
  describe "using Simple class" $ do
    it "should be able to type check method3" $ do
      Just cls <- getClassFromTestPool "Simple"
      let Just m = cls ^. classMethod "method3:(I)I"
      expected_m m [ B.VTInteger ]
        . Just $
        [ [ B.VTInteger ]
        , []
        ]

    it "should be able to type check method2" $ do
      Just cls <- getClassFromTestPool "Simple"
      expected_m (cls ^?! classMethod "method2:()V" . _Just)
        [] . Just $
        [ [ ]
        ]

    it "should be able to type check main" $ do
      Just cls <- getClassFromTestPool "Simple"
      expected_m (cls ^?! classMethod "main:([Ljava/lang/String;)V" . _Just)
        [] . Just $
        [ [ ]
        ]
