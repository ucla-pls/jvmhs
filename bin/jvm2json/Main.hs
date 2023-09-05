{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

import Conedec

import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable
import Jvmhs (deserializeClass)
import Jvmhs.Format.Codec
import Options.Applicative

data Action
  = OutputJSON

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
      bytes <- getClassFileBytes config
      case deserializeClass True bytes of
        Right cls -> do
          v <- withError fail $ pure <$> toEncodingClass cls
          BL.putStrLn (Aeson.encodingToLazyByteString v)
        Left err ->
          print err

getClassFileBytes :: Config -> IO BL.ByteString
getClassFileBytes Config{..} = do
  BL.readFile classFile
