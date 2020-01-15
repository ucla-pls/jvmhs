{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Inspection.ClassNamesSpec where

import           SpecHelper

-- containers
import qualified Data.Set                      as Set

-- jvmhs
import           Jvmhs.Inspection.ClassNames

spec :: Spec
spec = describe "classNamesOfClass" $ do
  cls <- runIO (getClassFromTestPool "Annotations")
  it "should list all classes in Annotations" $ do
    classNames (_Just . classNamesOfClass) cls `shouldBe` Set.fromList
      [ "Annotations"
      , "Annotations$Annotated"
      , "Annotations$B"
      , "Annotations$TestNotation"
      , "Annotations$TestType"
      , "Annotations$TestTypes"
      , "AnnotationsAnnotated"
      , "java/io/IOException"
      , "java/lang/Comparable"
      , "java/lang/Exception"
      , "java/lang/Object"
      , "java/lang/Throwable"
      , "java/util/List"
      ]
