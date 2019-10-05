{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.ClassReaderTest where

import SpecHelper

import Jvmhs.ClassReader

spec_asClassName :: Spec
spec_asClassName = do
  it "can read a file" $ do
    let x = asClassName "java/lang/String.class"
    x `shouldBe` Just "java/lang/String"
