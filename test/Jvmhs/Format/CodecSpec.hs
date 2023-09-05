{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Jvmhs.Format.CodecSpec where

import SpecHelper

import Autodocodec
import Autodocodec.Schema
import Autodocodec.Yaml
import qualified Data.Aeson as JSON
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Jvmhs.Format.Codec
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropFileName, (</>))
import Test.Hspec (focus)

spec :: Spec
spec = pure ()

--   spec_classname
--   spec_cmpopr
--   spec_typename
--   spec_bytecodeinst
--
-- expected' :: FilePath -> (FilePath -> IO ()) -> IO ()
-- expected' fp fn = do
--   let fp' = "test/expected/codec" </> fp
--   createDirectoryIfMissing True (dropFileName fp')
--   fn fp'
--
-- expected :: FilePath -> Text.Text -> IO ()
-- expected fp t = do
--   expected' fp (`Text.writeFile` t)
--
-- expectedSchema :: String -> JSONCodec a -> Spec
-- expectedSchema f a = it "should render schema" do
--   expected (f <> ".txt") $ renderPlainSchemaVia a
--
-- expectedJsonSchema :: String -> JSONCodec a -> Spec
-- expectedJsonSchema f a =
--   it "should render json schema" do
--     expected' (f <> ".json") (`JSON.encodeFile` jsonSchemaVia a)
--
-- spec_classname :: Spec
-- spec_classname = describe "ClassName" do
--   expectedSchema "classname" codecClassName
--
-- spec_cmpopr :: Spec
-- spec_cmpopr = describe "ComOpr" do
--   expectedSchema "cmpopr" codecCmpOpr
--
-- spec_typename :: Spec
-- spec_typename = describe "TypeName" do
--   expectedSchema "typename" codecTypeName
--
-- spec_bytecodeinst :: Spec
-- spec_bytecodeinst = describe "ByteCodeInst" do
--   expectedSchema "bytecodeinst" codecByteCodeInst
--   expectedJsonSchema "bytecodeinst" codecByteCodeInst
