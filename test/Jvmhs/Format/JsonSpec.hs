{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jvmhs.Format.JsonSpec where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Either
import Data.Text
import Jvmhs
import Jvmhs.Format.Json
import SpecHelper

import System.Process.Typed

import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = pure ()

--   do
--   forEveryClassIt "should print nice json" \c -> do
--     let file =
--           "test/expected/"
--             <> unpack (c ^. className . fullyQualifiedName)
--             <> ".json"
--     s <- readProcessStdout_ (proc "jq" ["."] & setStdin (byteStringInput $ encode (toJSONClass c)))
--     BL.writeFile file s
--     x :: Either String Value <- eitherDecodeFileStrict' file
--     x `shouldSatisfy` isRight
--     (parseEither parseJSONClass =<< x) `shouldBe` Right c
