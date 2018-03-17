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
      loadClass (strCls "DoesNotExist")
    fmap fst x @?=
      Left (ErrorWhileReadingClass (strCls "DoesNotExist") ClassNotFound)

simple_test :: TestTree
simple_test =
  testCaseSteps "Reading class file" $ \step -> do
    eu <- runHierarchyInClassPath classpath $ do
      c <- loadClass (strCls "Simple")
      liftIO $ do
        step "Test that the class name is 'Simple'"
        (strCls "Simple" @?= (c^.className))

        step "Check the class names referred to in the class"
        let clss = c^.classNames.to S.singleton
        clss @=? S.fromList
          [ strCls "Simple"
          , strCls "java/lang/Object"
          , strCls "java/lang/String"
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
          c' <- loadClass (strCls "Simple")
          liftIO $ c @?= c'
        fmap fst e @?= Right ()
      Left err ->
        fail (show err)

writing_test :: TestTree
writing_test =
  testCase "Altering a class file" $ do
    eu <- runHierarchyInClassPath classpath $ do
      c <- loadClass (strCls "Simple")
      liftIO $ do
        -- Rename class in all contents of the class
        let u = c & classNames.which(is (strCls "Simple"))
                  .~ strCls "test/Updated"

        -- Check that the field has changed value.
        JTClass (strCls "test/Updated") @=?
          u^?!classFieldsWhere(fieldName.is "field").fieldType

        -- Let's save the new class to the "output folder"
        writeClass "test/output" u

        return u

    case eu of
      Right (u, _) -> do
        e <- runHierarchyInClassPath ["test/output"] $ do
          u' <- loadClass (strCls "test/Updated")
          liftIO $ u @?= u'
        fmap fst e @?= Right ()
      Left err ->
        fail (show err)
