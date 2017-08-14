{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.TypeTest where


import           SpecHelper

import           Language.JVM.Type
import           Language.JVM.ClassName

spec_typeFromText :: Spec
spec_typeFromText = do
  it "fails on unknown values" $
    typeFromText "h" `shouldBe` Left "Unknown char 'h'"
  it "fails on incomplete patterns" $
    typeFromText "BB" `shouldBe` Left "Unread text \"B\""
  it "can parse a byte" $
    typeFromText "B" `shouldBe` Right Byte
  it "can parse a class" $
    typeFromText "Ljava/lang/String;" `shouldBe`
      Right (Class (ClassName "java/lang/String"))
  it "can parse an array of chars" $
    typeFromText "[C" `shouldBe`
      Right (Array Char)
  it "can parse an array of strings" $
    typeFromText "[Ljava/lang/String;" `shouldBe`
      Right (Array (Class (ClassName "java/lang/String")))

spec_methodDescriptorFromText :: Spec
spec_methodDescriptorFromText = do
  it "parses a void method description" $
    methodDescriptorFromText "(B)V" `shouldBe`
      Right (MethodDescriptor [ Byte ] Nothing)
  it "fails on bad a description" $
    methodDescriptorFromText "(B)Q" `shouldBe`
      Left "Expected 'V' but got 'Q'"
  it "parses complex descriptions" $
    methodDescriptorFromText "(BLjava/lang/String;)Ljava/lang/Object;" `shouldBe`
      Right ( MethodDescriptor
              [ Byte , Class (ClassName "java/lang/String")]
              (Just . Class . ClassName $ "java/lang/Object")
            )
