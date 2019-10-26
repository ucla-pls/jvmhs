{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.TypeCheckSpec where

import           Jvmhs
import           Data.Either
import           Data.Foldable
-- import           Jvmhs.Data.Code
import           Jvmhs.TypeCheck
-- import           Jvmhs.Data.Named
-- import           Jvmhs.Data.Code
-- import           Jvmhs.TypeCheck
import           SpecHelper

-- import           Text.Printf

-- import           Data.Maybe

-- import qualified Data.Vector as V
-- import qualified Language.JVM.Attribute.StackMapTable as B
-- import qualified Language.JVM.ByteCode as B


spec :: Spec
spec = do
  describe "typecheck" $ do
    mhry <- runIO $ getJREHierachy [] "stdlib-stubs.bin"
    forM_ mhry $ \hry -> do
      withJREClassMethods [] "java/lang/String" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/lang/Object" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/util/ArrayList" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/util/HashMap" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/lang/Enum" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/util/function/BiConsumer" "can typecheck" $
        doesTypeCheck hry

      withJREClassMethods [] "java/util/function/BiConsumer" "can typecheck" $
        doesTypeCheck hry

  describe "typecheck-local" $ do
    let cp = ["test/data/bigtest/classes", "test/data/bigtest/lib"]
    mhry <- runIO $ getJREHierachy cp "bigtest-stubs.bin"

    forM_ mhry $ \hry -> do
      withJREClassMethods cp "DungeonGame" "can typecheck" $
        doesTypeCheck hry

  where
    doesTypeCheck :: Hierarchy -> AbsMethodId -> Method -> IO ()
    doesTypeCheck hry mn mth =
      forM_ (mth^.methodCode) $ \code -> do
        let r = typeCheck hry mn (mth^.methodAccessFlags.contains MStatic)  code
        r `shouldSatisfy` isRight

    -- doesTypeCheck' :: Hierarchy -> AbsMethodId -> Method -> IO ()
    -- doesTypeCheck' hry mn mth =
    --   forM_ (mth^.methodCode) $ \code -> do
    --     r <- typeCheckDebug hry mn (mth^.methodAccessFlags.contains MStatic)  code
    --     r `shouldSatisfy` isRight



-- expected_m :: Method -> [ TypeInfo ] -> Maybe [ [ TypeInfo ] ] -> Expectation
-- expected_m m locals stack = do
--   let res = checkMethod m
--   res `shouldBe` V.fromList . map (mkStack locals) <$> stack

--   case res of
--     Just s -> do
--       reduceStackMap s `shouldBe` m ^? methodCode . _Just . codeStackMap . _Just
--     Nothing ->
--       return ()


-- spec_checkMethod :: Spec
-- spec_checkMethod = do
--   describe "using Simple class" $ do
--     it "should be able to type check method3" $ do
--       Just cls <- getClassFromTestPool "Simple"
--       let Just m = cls ^. classMethod "method3:(I)I"
--       expected_m m [ B.VTInteger ]
--         . Just $
--         [ [ B.VTInteger ]
--         , []
--         ]

--     it "should be able to type check method2" $ do
--       Just cls <- getClassFromTestPool "Simple"
--       expected_m (cls ^?! classMethod "method2:()V" . _Just)
--         [] . Just $
--         [ [ ]
--         ]

--     it "should be able to type check main" $ do
--       Just cls <- getClassFromTestPool "Simple"
--       expected_m (cls ^?! classMethod "main:([Ljava/lang/String;)V" . _Just)
--         [] . Just $
--         [ [ ]
--         ]
