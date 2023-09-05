{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

import Conedec

import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Internal as Aeson
import qualified Data.Aeson.Parser as Aeson
import qualified Data.Aeson.Parser.Internal as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Jvmhs (deserializeClass, serializeClass)
import Jvmhs.Format.Codec
import Options.Applicative

data Action
  = OutputJSON
  | InputJSON

data Config = Config
  { classFile :: FilePath
  }

parseConfig :: Parser (Config, Action)
parseConfig = do
  classFile <-
    argument str . fold $
      [ help "the path to the classfile"
      , metavar "CLASSFILE"
      ]
  pure (Config{..}, OutputJSON)

main :: IO ()
main = do
  (config, act) <-
    execParser . info (parseConfig <**> helper) . fold $
      [ progDesc "a tool for converting between java classfile and json"
      ]
  case act of
    OutputJSON -> do
      bytes <- readClassFileBytes config
      case deserializeClass True bytes of
        Right cls -> do
          v <- withError fail $ pure <$> toEncodingClass cls
          BL.putStrLn (Aeson.encodingToLazyByteString v)
        Left err ->
          print err
    InputJSON -> do
      bytes <- BL.getContents
      case Aeson.eitherDecodeWith Aeson.jsonEOF' (Aeson.iparse parseJSONClass) bytes of
        Right cls -> do
          writeClassFileBytes config (serializeClass cls)
        Left err ->
          print err
      writeClassFileBytes config bytes

readClassFileBytes :: Config -> IO BL.ByteString
readClassFileBytes Config{..} = do
  BL.readFile classFile

writeClassFileBytes :: Config -> BL.ByteString -> IO ()
writeClassFileBytes Config{..} bs = do
  BL.writeFile classFile bs
