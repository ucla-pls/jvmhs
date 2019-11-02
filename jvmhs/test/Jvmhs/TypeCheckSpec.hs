{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.TypeCheckSpec where

import           Data.Foldable

import           SpecHelper

import           Jvmhs
import           Jvmhs.TypeCheck

spec :: Spec
spec = do
  withJREHierarchy . describe "typecheck" $ do
      withJREClassMethods [] "java/lang/String"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/lang/Object"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/util/ArrayList"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/util/HashMap"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/lang/Enum"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/util/function/BiConsumer"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/util/function/BiConsumer"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "com/apple/laf/ScreenMenuItemCheckbox"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "java/beans/VetoableChangeSupport"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "sun/rmi/transport/DGCImpl_Skel"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "javax/management/remote/rmi/_RMIConnection_Stub"
        "can typecheck" doesTypeCheck

      -- withJREClassMethods [] "sun/reflect/annotation/AnnotationParser"
      --   "can typecheck" doesTypeCheck

      withJREClassMethods [] "sun/print/RasterPrintJob"
        "can typecheck" doesTypeCheck

      withJREClassMethods [] "com/sun/beans/TypeResolver"
        "can typecheck" doesTypeCheck

    where
    doesTypeCheck :: AbsMethodId -> Method -> Hierarchy -> IO ()
    doesTypeCheck mn mth hry =
      forM_ (mth^.methodCode) $ \code -> do
        case typeCheck hry mn (mth^.methodAccessFlags.contains MStatic) code of
          (Just (i, err), res) -> do
            forM_ [max (i-10) 0..i] $ \j ->
              debugInfo j code res
            expectationFailure $ "found type error: " ++ show i ++ " "++ show err
          (Nothing, _) ->
            return ()
    -- doesTypeCheck' :: AbsMethodId -> Method -> Hierarchy -> IO ()
    -- doesTypeCheck' mn mth hry =
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
