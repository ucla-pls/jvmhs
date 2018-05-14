{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Main where

import           Control.DeepSeq            (NFData, force)
import           Control.Lens               hiding (argument)
import           Crypto.Hash.SHA256         (hashlazyAndLength)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Either
import           Data.List                  as List
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           GHC.Generics               (Generic)
import           System.IO                  (stderr, hPrint)
-- import qualified Data.ByteString.Lazy as BL
-- import           Data.Foldable
-- import           Data.Text.IO               as Text
import           System.Console.Docopt
import           System.Environment         (getArgs)

import           Control.Monad
-- import Control.Monad.IO.Class

import           Jvmhs

patterns :: Docopt
patterns = [docopt|
javaq version 0.0.1

Usage:
  javaq [options] [-] [<classname>...]

Options:
  --cp=<classpath>         The classpath to search for classes.
  --stdlib                 Also analyse the stdlib.
  --jre=<jre>              The location of the stdlib.
  -f, --format=<format>    The Output format, see Output formats.
  -h, --help               Display this help page.

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

    json[s]-counted:
      like listed but instead of returning lists it mearly count the number
      of entries in the list.
  csv:
    Output a csv file correspondin to the content of json-counted
|]

data DTOType
  = FullDTO
  | ListedDTO
  | CountDTO
  deriving (Show, Eq)

data OutputFormat
  = OutputJSON DTOType
  | OutputJSONs DTOType
  | OutputCSV
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
  { _coName       :: ! ClassName
  , _coSha256     :: ! Text.Text
  , _coSize       :: ! Int
  , _coSuper      :: ! ClassName
  , _coInterfaces :: ! ([ ClassName ])
  , _coFields     :: ! ([ FieldId ])
  , _coMethods    :: ! ([ MethodId ])
  } deriving (Show, Generic, NFData)

data ClassCount = ClassCount
  { _ccName       :: ! ClassName
  , _ccSha256     :: ! Text.Text
  , _ccSize       :: ! Int
  , _ccSuper      :: ! ClassName
  , _ccInterfaces :: ! Int
  , _ccFields     :: ! Int
  , _ccMethods    :: ! Int
  } deriving (Show, Generic, NFData)

makeLenses ''ClassOverview
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassOverview)

makeLenses ''ClassCount
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassCount)

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

parseOutputFormat :: String -> Maybe OutputFormat
parseOutputFormat str =
  case str of
    "jsons-listed"  -> Just $ OutputJSONs ListedDTO
    "json-listed"   -> Just $ OutputJSON ListedDTO

    "jsons-full"    -> Just $ OutputJSONs FullDTO
    "json-full"     -> Just $ OutputJSON FullDTO

    "jsons-counted" -> Just $ OutputJSONs CountDTO
    "json-counted"  -> Just $ OutputJSON CountDTO

    "csv"           -> Just $ OutputCSV

    _               -> Nothing


parseConfig :: Arguments -> IO Config
parseConfig args = do
  let format = getArgWithDefault args "jsons-listed" (longOption "format")
  oformat <- case parseOutputFormat format of
    Nothing -> error ("Could not recognize output-format: '" ++ format ++ "'")
    Just fs -> return fs

  classnames <- readClassNames

  return $ Config
    { _cfgClassPath =
        case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
          [] -> ["."]
          as -> as
    , _cfgUseStdlib = isPresent args (longOption "stdlib")
    , _cfgJre = getArg args (longOption "jre")
    , _cfgClassNames = classnames
    , _cfgFormat = oformat
    }

  where
    classnames' = getAllArgs args $ argument "classname"
    readClassNames = do
      names <-
        if isPresent args (command "-")
        then do (classnames' ++) . lines <$> getContents
        else return classnames'
      return . map strCls $ names

main :: IO ()
main = do
  args' <- parseArgs patterns <$> getArgs
  case args' of
    Left msg -> do
      hPrint stderr msg
      exitWithUsage patterns
    Right args -> do
      if isPresent args (longOption "help")
        then do
          exitWithUsage patterns
        else do
          cfg <- parseConfig args
          decompile cfg

-- | Decompile and print a classfile to stdout
decompile :: Config -> IO ()
decompile cfg = do
  classReader <- preload =<< createClassLoader cfg
  classnames <-
    case cfg ^. cfgClassNames of
      [] -> map fst <$> classes classReader
      a  -> return a
  let
    onEachClass ::
      (NFData a, NFData x, Show x)
      => (ClassName -> IO (Either x a))
      -> ([a] -> IO ())
      -> IO ()
    onEachClass doeach dofinal = do
      (fails,succs) <- partitionEithers <$> forM classnames (
        \cn -> force (over _Left (cn,)) <$> doeach cn
        )
      forM_ fails (hPrint stderr)
      dofinal succs
    readClassOverview cn = do
      bytes <- getClassBytes classReader cn
      return $ do
        b <- bytes
        clsf <- readClassFile' b
        let
          cls = convertClass clsf
          (hsh, lth) = hashlazyAndLength b
        return $! ClassOverview
          (cls^.className)
          (Text.decodeUtf8 . B16.encode $ hsh)
          (fromIntegral lth)
          (cls^.classSuper)
          (cls^.classInterfaces)
          (cls^..classFields.folded.toFieldId)
          (cls^..classMethods.folded.toMethodId)
    readClassCount cn = do
      co' <- readClassOverview cn
      return $ do
        co <- co'
        return $! ClassCount
          (co^.coName)
          (Text.take 7 $ co^.coSha256)
          (co^.coSize)
          (co^.coSuper)
          (length $ co^.coInterfaces)
          (length $ co^.coFields)
          (length $ co^.coMethods)
  case cfg ^. cfgFormat of
    OutputJSON dto ->
      case dto of
        FullDTO ->
          onEachClass (readClass classReader) $ BS.putStrLn . encode
        ListedDTO ->
          onEachClass readClassOverview $ BS.putStrLn . encode
        CountDTO ->
          onEachClass readClassCount $ BS.putStrLn . encode
    OutputJSONs dto ->
      case dto of
        FullDTO ->
          onEachClass (readClass classReader >=> encode') (const $ return ())
        ListedDTO ->
          onEachClass (readClassOverview >=> encode') (const $ return ())
        CountDTO ->
          onEachClass (readClassCount >=> encode') (const $ return ())
    OutputCSV -> do
      putStrLn "class,sha256,size,super,interfaces,fields,methods"
      onEachClass
        (\cn -> do
          cc' <- readClassCount cn
          case cc' of
            Right cc ->
              Right <$> (putStrLn . List.intercalate "," $
                [ Text.unpack (cc^.ccName.fullyQualifiedName)
                , Text.unpack (cc^.ccSha256)
                , show (cc^.ccSize)
                , Text.unpack (cc^.ccSuper.fullyQualifiedName)
                , show (cc^.ccInterfaces)
                , show (cc^.ccFields)
                , show (cc^.ccMethods)
                ])
            Left msg -> return $ Left msg
        ) (const $ return ())
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
