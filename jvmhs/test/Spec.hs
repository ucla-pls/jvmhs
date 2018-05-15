{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Data.List (isInfixOf)
import System.Exit
import System.FilePath
import System.Directory
import System.Process
import qualified Data.Set as S
import Control.Monad.IO.Class
import Control.Lens

import qualified Data.Text as Text

import Jvmhs
import Jvmhs.LensHelpers

main :: IO ()
main = do
  test <- tests
  defaultMain test

tests :: IO TestTree
tests = do
  regression_tests <- findRegressionTests "test/regression"
  return $ testGroup "Tests"
    [ class_not_found
    , simple_test
    , writing_test
    , regression_tests
    ]

findRegressionTests :: FilePath -> IO TestTree
findRegressionTests filePath = do
  files <- map toClass . filter isClass <$> recursiveContents filePath
  return $ testGroup "Regression tests" (map (testRegressionClass filePath) files)
  where
    isClass = (== ".class") . takeExtension
    toClass = strCls . makeRelative filePath . dropExtension


testRegressionClass :: FilePath -> ClassName -> TestTree
testRegressionClass fp cn = do
  testCase (Text.unpack $ cn^.fullyQualifiedName) $ do
    Right x <- runHierarchyInClassPathOnly [ fp ] $ loadClass cn
    writeClass "test/output" x

    let cp = (shell $ "java -cp test/output " ++ (Text.unpack $ cn^.fullyQualifiedName))
    (ec, _, sout) <- readCreateProcessWithExitCode cp ""
    case ec of
      ExitFailure n
        | not $ "java.lang.NoClassDefFoundError" `isInfixOf` sout ->
          assertFailure ("Error " ++ show n ++ ": \n" ++ sout)
      _ ->
        return ()

classpath :: [ FilePath ]
classpath = [ "test/data/classes" ]

class_not_found :: TestTree
class_not_found =
  testCase "Class Not Found Error" $ do
    x <- runHierarchyInClassPathOnly classpath $ do
      loadClass (strCls "DoesNotExist")
    x @?= Left (ErrorWhileReadingClass (strCls "DoesNotExist") ClassNotFound)

simple_test :: TestTree
simple_test =
  testCaseSteps "Reading class file" $ \step -> do
    eu <- runHierarchyInClassPathOnly classpath $ do
      c <- loadClass (strCls "Simple")
      liftIO $ do
        step "Test that the class name is 'Simple'"
        (strCls "Simple" @?= (c^.className))

        step "Check the class names referred to in the class"
        let clss = c^.classNames.to S.singleton
        clss @=? S.fromList
          [ "Simple"
          , "java/io/PrintStream"
          , "java/lang/Object"
          , "java/lang/String"
          , "java/lang/System"
          ]

        step "The field 'field' needs to point to null"
        Nothing @?=
          c^?!classFieldsWhere(fieldName.is "field").fieldValue

        step "Test if we can write a class"
        writeClass "test/output" c

        return c

    case eu of
      Right c -> do
        step "Test if we can reread it"
        e <- runHierarchyInClassPath ["test/output"] $ do
          c' <- loadClass (strCls "Simple")
          liftIO $ c @?= c'
        e @?= Right ()
      Left err ->
        fail (show err)

writing_test :: TestTree
writing_test =
  testCase "Altering a class file" $ do
    eu <- runHierarchyInClassPathOnly classpath $ do
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
      Right u -> do
        e <- runHierarchyInClassPath ["test/output"] $ do
          u' <- loadClass (strCls "test/Updated")
          liftIO $ u @?= u'
        e @?= Right ()
      Left err ->
        fail (show err)



folderContents :: FilePath -> IO [ FilePath ]
folderContents fp =
  map (fp </>) <$> listDirectory fp

recursiveContents :: FilePath -> IO [ FilePath ]
recursiveContents fp = do
  test <- doesDirectoryExist fp
  (fp:) <$> if test then do
    content <- folderContents fp
    concat <$> mapM recursiveContents content
  else return []
