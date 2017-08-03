{-# LANGUAGE OverloadedStrings #-}
import           Language.JVM.Binary.Attribute (Attribute(..))
import           Language.JVM.Binary.ClassFile


import           Data.Binary            (decode, encode)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString as BS


import qualified Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck  as QC


main :: IO ()
main = do
  putStrLn ".."
  test <- testSpec "jvmhs" classfileSpec
  Test.Tasty.defaultMain (Test.Tasty.testGroup "All of it" [ test, encodedecode ])

classfileSpec :: Spec
classfileSpec = beforeAll (LBS.readFile "test-suite/project/Main.class") $ do
  it "can read the bytestring" $
    \bs -> let classfile = decode bs
           in (magicNumber classfile) `shouldBe` 3405691582

  it "can encode and decode attributes" $
    \_ -> let attr = Attribute 1 (BC.pack "Hello")
           in (decode . encode ) attr `shouldBe` attr

  it "will encode and decode back to a class file" $ \bs ->
    let classfile = decode bs :: ClassFile
        encoding = encode classfile
    in (methods classfile) `shouldBe` (methods $ decode (LBS.append encoding (LBS.repeat 0)))

  it "will encode the data back to the same format" $ \bs ->
    let classfile = decode bs :: ClassFile
    in bs `shouldBe` encode classfile


encodedecode :: Test.Tasty.TestTree
encodedecode =
  Test.Tasty.testGroup "encode . decode == id"
  [ QC.testProperty "Attribute" $ \(ArbAttribute attr) ->
      (decode . encode) attr == attr
  ]

newtype ArbAttribute =
  ArbAttribute Attribute deriving (Show, Eq)

instance Arbitrary ArbAttribute where
  arbitrary = do
    index <- arbitrary
    len <- choose (0, 50)
    bs <- BS.pack <$> sequence (replicate len arbitrary)
    return $ ArbAttribute (Attribute index bs)
