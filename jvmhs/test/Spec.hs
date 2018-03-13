{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Lens

import Jvmhs
import Jvmhs.LensHelpers

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ class_not_found
  , simple_test
  , writing_test
  ]

classpath :: [ FilePath ]
classpath = [ "test/data/classes" ]


class_not_found :: TestTree
class_not_found =
  testCase "Class Not Found Error" $ do
    x <- runHierarchyInClassPath classpath $ do
      loadClass (ClassName "DoesNotExist")
    fmap fst x @?=
      Left (ErrorWhileReadingClass (ClassName "DoesNotExist") ClassNotFound)

simple_test :: TestTree
simple_test =
  testCaseSteps "Reading class file" $ \step -> do
    eu <- runHierarchyInClassPath classpath $ do
      c <- loadClass (ClassName "Simple")
      liftIO $ do
        step "Test that the class name is 'Simple'"
        (ClassName "Simple" @?= (c^.className)

        step "Check the class names referred to in the class"
        let clss = c^.classNames.to S.singleton
        clss @=? S.fromList
          [ ClassName "Simple"
          , ClassName "java/lang/Object"
          , ClassName "java/lang/String"
          ]

        step "The field 'field' needs to point to null"
        Nothing @?=
          c^?!classFieldsWhere(fieldName.is "field").fieldConstantValue

        step "Test if we can write a class"
        writeClass "test/output" c

        return c

    case eu of
      Right (c, _) -> do
        step "Test if we can reread it"
        e <- runHierarchyInClassPath ["test/output"] $ do
          c' <- loadClass (ClassName "Simple")
          liftIO $ c @?= c'
        fmap fst e @?= Right ()
      Left err ->
        fail (show err)

writing_test :: TestTree
writing_test =
  testCase "Altering a class file" $ do
    eu <- runHierarchyInClassPath classpath $ do
      c <- loadClass (ClassName "Simple")
      liftIO $ do
        -- Rename class in all contents of the class
        let u = c & classNames.which(is (ClassName "Simple"))
                  .~ ClassName "test/Updated"

        -- Check that the field has changed value.
        JTClass (ClassName "test/Updated") @=?
          u^?!classFieldsWhere(fieldName.is "field").fieldType

        -- Let's save the new class to the "output folder"
        writeClass "test/output" u

        return u

    case eu of
      Right (u, _) -> do
        e <- runHierarchyInClassPath ["test/output"] $ do
          u' <- loadClass (ClassName "test/Updated")
          liftIO $ u @?= u'
        fmap fst e @?= Right ()
      Left err ->
        fail (show err)
