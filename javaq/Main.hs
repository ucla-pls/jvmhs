{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens               hiding (argument)
import           Data.Aeson
import           Crypto.Hash.SHA256 (hashlazyAndLength)
import           Data.Aeson.TH
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BS
-- import qualified Data.ByteString.Lazy as BL
-- import           Data.Foldable
-- import           Data.Text.IO               as Text
import           System.Console.Docopt
import           System.Environment         (getArgs)

import Control.Monad
-- import Control.Monad.IO.Class

import           Jvmhs

patterns :: Docopt
patterns = [docopt|
javaq version 0.0.1

Usage:
  javaq (-h | --help)
  javaq [options]
  javaq [options] -- <classname>...

Options:
  --cp=<classpath>         The classpath to search for classes.
  --stdlib                 Also analyse the stdlib.
  --jre=<jre>              The location of the stdlib.
  -f, --format=<format>    The Output format, see Output formats.

Output Formats:
  We support a list of output formats, the formats can be sepefied like this:
  base format '-' data type.

  json[s]:
    Output as json. If an s is added, then instead of a single json array
    then the classes will be outputted a line at a time, as we find them.

    json[s]-full:
      returns the full data object with all its information. Ideally you can
      recreate the bytecode from this representation. It is currently
      a work in progress, and are missing key fields.

    json[s]-listed (default):
      returns only the name of the class, the fields, and the methods.
      It also provides information about sizes of the different classes.

    json[s]-count:
      like listed but instead of returning lists it mearly count the number
      of entries in the list.
|]

data DTOType
  = FullDTO
  | ListedDTO
  | CountDTO
  deriving (Show, Eq)

data OutputFormat
  = OutputJSON DTOType
  | OutputJSONs DTOType
  deriving (Show)

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath  :: ClassPath
  , _cfgJre        :: Maybe FilePath
  , _cfgUseStdlib  :: Bool
  , _cfgClassNames :: [ ClassName ]
  , _cfgFormat     :: OutputFormat
  } deriving (Show)

makeLenses ''Config

data ClassOverview = ClassOverview
  { _coName :: ClassName
  , _coSize :: Int
  , _coSha256 :: Text.Text
  , _coSuper :: ClassName
  , _coInterfaces :: [ ClassName ]
  , _coFields :: [ FieldId ]
  , _coMethods :: [ MethodId ]
  } deriving (Show)

makeLenses ''ClassOverview
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassOverview)

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat str =
  case str of
    "jsons-listed" -> Just $ OutputJSONs ListedDTO
    "json-listed"  -> Just $ OutputJSON ListedDTO

    "jsons-full"   -> Just $ OutputJSONs FullDTO
    "json-full"    -> Just $ OutputJSON FullDTO

    "jsons-count"  -> Just $ OutputJSONs CountDTO
    "json-count"   -> Just $ OutputJSON CountDTO

    _ -> Nothing


parseConfig :: Arguments -> IO Config
parseConfig args = do
  let format = getArgWithDefault args "jsons-listed" (longOption "format")
  oformat <- case parseOutputFormat format of
    Nothing -> error ("Could not recognize output-format: '" ++ format ++ "'")
    Just fs -> return fs

  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgClassNames = strCls <$> getAllArgs args (argument "classname")
    , _cfgFormat = oformat
    }

main :: IO ()
main = do
  args' <- parseArgs patterns <$> getArgs
  case args' of
    Left msg -> do
      print msg
      exitWithUsage patterns
    Right args -> do
      cfg <- parseConfig args
      decompile cfg

-- | Decompile and print a classfile to stdout
decompile :: Config -> IO ()
decompile cfg = do
  classReader <- preload =<< createClassLoader cfg
  classnames <-
    case cfg ^. cfgClassNames of
      [] -> map fst <$> classes classReader
      a -> return a
  let
    onEachClass doeach dofinal = do
      e <- sequence <$> forM classnames doeach
      case e of
        Left msg -> error (show msg)
        Right ls -> dofinal ls
    readClassOverview cn = do
      bytes <- getClassBytes classReader cn
      return $ do
        b <- bytes
        clsf <- readClassFile' b
        let
          cls = convertClass clsf
          (hsh, lth) = hashlazyAndLength b
        return $ ClassOverview
          (cls^.className)
          (fromIntegral lth)
          (Text.decodeUtf8 . B16.encode $ hsh)
          (cls^.classSuper)
          (cls^.classInterfaces)
          (cls^..classFields.folded.toFieldID)
          (cls^..classMethods.folded.toMethodID)
  case cfg ^. cfgFormat of
    OutputJSON dto ->
      case dto of
        FullDTO ->
          onEachClass (readClass classReader) $ BS.putStrLn . encode
        ListedDTO ->
          onEachClass readClassOverview $ BS.putStrLn . encode
        CountDTO -> undefined
    OutputJSONs dto ->
      case dto of
        FullDTO ->
          onEachClass (readClass classReader >=> encode') (const $ return ())
        ListedDTO ->
          onEachClass (readClassOverview >=> encode') (const $ return ())
        CountDTO -> undefined

   where
     encode' :: ToJSON e => Either a e -> IO (Either a ())
     encode' =
       either (return . Left) (fmap Right . BS.putStrLn . encode)

-- | Create a class loader from the config
createClassLoader :: Config -> IO ClassLoader
createClassLoader cfg
  | cfg ^. cfgUseStdlib =
    case cfg ^. cfgJre of
      Nothing ->
        fromClassPath (cfg ^. cfgClassPath)
      Just jre ->
        fromJreFolder (cfg ^. cfgClassPath) jre
  | otherwise =
    return $ ClassLoader [] [] (cfg ^. cfgClassPath)
