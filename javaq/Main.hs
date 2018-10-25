{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Main where

-- prelude
import           Control.Monad
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import qualified Data.Set                     as Set
import qualified Data.Map.Strict              as Map
import           Data.String
import           System.Environment
import           System.IO

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BS

-- aeson
import           Data.Aeson

-- mtl
import           Control.Monad.Reader

-- text
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text

-- filepath
import           System.FilePath

-- optparse-applicative
import           Options.Applicative

-- ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as D

-- lens
import           Control.Lens                 hiding (argument)

-- jvmhs
import           Jvmhs

data OutputFormat
  = Stream (StreamFunction (ReaderT Config IO))
  | Group [Format]

data StreamFunction m
  = StreamContainer ( (ClassName, ClassContainer) -> m () )
  | StreamClassName ( ClassName -> m () )
  | StreamClass ( Class -> m () )

data Format = Format
  { formatName        :: String
  , formatDescription :: D.Doc
  , formatType        :: OutputFormat
  }
instance Show Format where
  show x = "Format { formatName = " ++ show (formatName x) ++ " , ...}"

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath      :: ClassPath
  , _cfgJre            :: FilePath
  , _cfgUseStdlib      :: Bool
  , _cfgComputeClosure :: Bool
  , _cfgFast :: Bool
  , _cfgFormat         :: Format
  -- , _cfgStubs          :: HierarchyStubs
  , _cfgClassNames     :: [ ClassName ]
  } deriving (Show)

makeLenses ''Config

getConfigParser :: [Format] -> IO (Parser (IO Config))
getConfigParser formats = do

  let format = head formats

  mclasspath <- lookupEnv "CLASSPATH"

  jre <-
    lookupEnv "JAVA_HOME" >>= \case
      Just javaHome -> return $ javaHome </> "jre"
      Nothing -> guessJre

  return $
    readConfig formats

    <$> option str
    ( long "cp"
      <> help "The classpath to search for classes. Defaults to $CLASSPATH."
      <> metavar "CLASSPATH"
      <> (Maybe.maybe mempty value $ mclasspath)
      <> showDefault
    )

    <*> option str
    ( long "jre"
      <> helpDoc
      ( Just
        $ "The jre folder to read stdlib from."
        D.</> "Defaults to $JAVA_HOME/jre"
        D.</> "or $(which java)/../jre." D.</> D.empty
      )
      <> metavar "JRE"
      <> value jre
      <> showDefault
    )

    <*> switch
    ( long "stdlib"
    <> help "Include the stdlib, jars from <jre>/lib and <jre>/lib/ext, on the classpath."
    )

    <*> switch
    ( long "closure"
    <> short 'C'
    <> help "Compute a closure over all the classes, and include them."
    )

    <*> switch
    ( long "fast"
      <> short 'f'
      <> ( helpDoc . Just $ (
             "Do not read any attributes, including the byte code instructions."
             <> "This can be much faster, but also changes the behavior of the formats, that compute closures."
             )
         )
    )

    <*> option str
    ( long "format"
      <> help "The output format, see Formats."
      <> metavar "FORMAT"
      <> value (formatName format)
      <> showDefault
    )

    <*> many
    ( argument str
      ( metavar "CLASS .."
      <> help "Classes to investigate, if none present all will be listed."
      )
    )
  where
    readConfig formats classpath jre useStdlib computeClosure fast format classNames = do
      let Just a = List.find ((format ==) . formatName) formats
      return $
        Config
        (splitClassPath classpath)
        jre
        useStdlib
        computeClosure
        fast
        a
        classNames

footerFromFormats :: [Format] -> D.Doc
footerFromFormats fts =
  D.nest 2 $
    D.text "Formats:"
    D.<$> "Here is a list of all formats that can be used with the"
    D.<+> "with the --format option."
    D.<$> D.empty
    D.<$> joinFormats fts (formatFormatter "")
  where
    joinFormats fts fn =
      D.vcat . map ((<> D.line) . fn) $ fts

    formatFormatter prefix (Format name desc type_) =
      D.nest 2 $
      let
        nameDoc = prefix D.<> D.text name
        title = D.green (nameDoc D.<> D.colon)
      in case type_ of
        Stream _ ->
          title D.<+> D.parens "stream"
          D.<$> desc
        Group fmts ->
          let prefix' = prefix D.<> D.text name D.<> "-"
          in
            title D.<+> D.parens ("default:" D.<+> (prefix' D.<> (D.text . formatName . head $ fmts)))
            D.<$> desc
            D.<$> D.empty
            D.<$> joinFormats fmts (formatFormatter (prefix D.<> D.text name D.<> "-"))

main :: IO ()
main = do
  parseConfig <- getConfigParser formats
  config <- join . execParser $
    info (parseConfig <**> helper)
    ( fullDesc
    <> header "javaq"
    <> footerDoc (Just (footerFromFormats formats))
    <> progDesc "A program that can inspect JVM ClassFiles"
    )
  runReaderT run config

run :: ReaderT Config IO ()
run = do
  classloader <- createClassLoader
  format <- view (cfgFormat . to formatType)
  runFormat classloader format

runFormat ::
  (ClassReader r)
  => r
  -> OutputFormat
  -> ReaderT Config IO ()
runFormat classloader = \case
  Stream sfn -> do
    preloaded' <- liftIO ( preload classloader )
    isFast <- view $ cfgFast
    let opts = (defaultFromReader preloaded') { keepAttributes = not isFast}

    void . flip runCachedClassPoolT opts $ do

      view cfgClassNames >>= \case
        [] -> return ()
        classNames -> do
          let classSet = Set.fromList classNames
          classSet' <- view cfgComputeClosure >>= \case
            True -> fst <$> computeClassClosure classSet
            False -> return classSet
          restrictTo classSet'

      case sfn of
        StreamClassName fn -> do
          mapM_ (lift . fn) =<< allClassNames
        StreamContainer fn -> do
          let cm = classReader opts  ^. classMap
          set <- Set.fromList <$> allClassNames
          mapM_ (\(cn, cl) -> lift $ fn (cn, cl))
            . Map.toList
            . fmap head
            . Map.restrictKeys cm $ set
        StreamClass fn -> do
          streamClasses fn

  Group ( f:_ ) ->
    runFormat classloader (formatType f)

formats =
  [ Format "list" "List classes available on the classpath"
  $ Stream . StreamClassName $ \cn -> liftIO $ do
      Text.putStrLn (cn ^. fullyQualifiedName)
  , Format "containers"
    ( "List classes and their containers, outputs in a tap-separated fashion."
    D.</> "To get a count of classes per container, try:"
    D.<$> D.indent 4 "> javaq --stdlib --cp /dev/null --format containers | cut -f 2 | sort | uniq -c"
    )
  $ Stream . StreamContainer $ \(cn, lo) -> liftIO $ do
      Text.putStr (cn ^. fullyQualifiedName)
      Text.putStr "\t"
      case lo of
        CCFolder (CFolder fp)           -> putStrLn fp
        CCJar (CJar fp _)               -> putStrLn fp
        CCEntry (CEntry (CJar fp _, _)) -> putStrLn fp
  , Format "json" "Output each class as json"
  $ Group
    [ Format "full" "Full output of the class"
    $ Stream . StreamClass $ liftIO . BS.putStrLn . encode
    ]
  ]

-- | Create a class loader from the config
createClassLoader :: (MonadReader Config m, MonadIO m) => m ClassLoader
createClassLoader =
  view cfgUseStdlib >>= \case
    True ->
      ( fromJreFolder
        <$> view cfgClassPath
        <*> view cfgJre
      ) >>= liftIO
    False ->
      ClassLoader [] [] <$> view cfgClassPath

-- import           Control.DeepSeq            (NFData, force)
-- import           Control.Lens               hiding (argument)
-- import           Control.Monad
-- import           Crypto.Hash.SHA256         (hashlazyAndLength)
-- import           Data.Aeson
-- import           Data.Aeson.TH
-- import qualified Data.ByteString.Base16     as B16
-- import           Data.Either
-- import           Data.List                  as List
-- import qualified Data.Text                  as Text
-- import qualified Data.Text.Encoding         as Text
-- import           GHC.Generics               (Generic)
-- import           System.Console.Docopt
-- import           System.Environment         (getArgs)
-- import           System.IO                  (hPrint, hPutStrLn, stderr)

-- import           Data.Maybe

-- import           Jvmhs

-- patterns :: Docopt
-- patterns = [docopt|
-- javaq version 0.0.1

-- Usage:
--   javaq [options] [-] [<classname>...]

-- Options:
--   --cp=<classpath>         The classpath to search for classes.
--   --stdlib                 Also analyse the stdlib.
--   --jre=<jre>              The location of the stdlib.
--   --stub=<stub>            Precalcualated stubs of libraries, to use in hierarcy analysis
--   --remove-attr            Remove all attributes
--   -f, --format=<format>    The Output format, see Output formats.
--   -h, --help               Display this help page.

-- Output Formats:
--   We support a list of output formats, the formats can be sepefied like this:
--   base format '-' data type.

--   json[s]:
--     Output as json. If an s is added, then instead of a single json array
--     then the classes will be outputted a line at a time, as we find them.

--     json[s]-full:
--       returns the full data object with all its information. Ideally you can
--       recreate the bytecode from this representation. It is currently
--       a work in progress, and are missing key fields.

--     json[s]-listed (default):
--       returns only the name of the class, the fields, and the methods.
--       It also provides information about sizes of the different classes.

--     json[s]-counted:
--       like listed but instead of returning lists it mearly count the number
--       of entries in the list.

--   csv:
--     Output a csv file correspondin to the content of json-counted

--   dot:
--     Output a dot graph. The default is the class graph.

--     dot-call:
--       A call graph.

--     dot-class:
--       A class dependecy graph.

--     dot-hierarchy:
--       A class hierarchy graph.

--   stub:
--     Produce stubs to use in a hierarchy analysis

--   defs:
--     Output definitions of all methods.
-- |]

-- data GraphForm
--   = GFSCC
--   | GFFull
--   deriving (Show, Eq)

-- data GraphType
--   = GTClass
--   | GTCall
--   | GTHierarchy
--   deriving (Show, Eq)

-- data DTOType
--   = FullDTO
--   | ListedDTO
--   | CountDTO
--   deriving (Show, Eq)

-- data OutputFormat
--   = OutputJSON DTOType
--   | OutputJSONs DTOType
--   | OutputCSV
--   | OutputDot GraphType GraphForm
--   | OutputStubs
--   | OutputDefinitions
--   deriving (Show)


-- data ClassOverview = ClassOverview
--   { _coName       :: ! ClassName
--   , _coSha256     :: ! Text.Text
--   , _coSize       :: ! Int
--   , _coSuper      :: ! (Maybe ClassName)
--   , _coInterfaces :: ! ([ ClassName ])
--   , _coFields     :: ! ([ FieldId ])
--   , _coMethods    :: ! ([ MethodId ])
--   } deriving (Show, Generic, NFData)

-- data ClassCount = ClassCount
--   { _ccName       :: ! ClassName
--   , _ccSha256     :: ! Text.Text
--   , _ccSize       :: ! Int
--   , _ccSuper      :: ! (Maybe ClassName)
--   , _ccInterfaces :: ! Int
--   , _ccFields     :: ! Int
--   , _ccMethods    :: ! Int
--   } deriving (Show, Generic, NFData)

-- makeLenses ''ClassOverview
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassOverview)

-- makeLenses ''ClassCount
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassCount)

-- getArgOrExit :: Arguments -> Option -> IO String
-- getArgOrExit = getArgOrExitWith patterns

-- parseOutputFormat :: String -> Maybe OutputFormat
-- parseOutputFormat str =
--   case str of
--     "jsons-listed"  -> Just $ OutputJSONs ListedDTO
--     "json-listed"   -> Just $ OutputJSON ListedDTO

--     "jsons-full"    -> Just $ OutputJSONs FullDTO
--     "json-full"     -> Just $ OutputJSON FullDTO

--     "jsons-counted" -> Just $ OutputJSONs CountDTO
--     "json-counted"  -> Just $ OutputJSON CountDTO

--     "csv"           -> Just $ OutputCSV

--     "dot"           -> Just $ OutputDot GTClass GFFull
--     "dot-scc"       -> Just $ OutputDot GTClass GFSCC

--     "dot-call"      -> Just $ OutputDot GTCall GFFull
--     "dot-call-scc"  -> Just $ OutputDot GTCall GFSCC

--     "dot-hierarchy" -> Just $ OutputDot GTHierarchy GFFull

--     "stub"          -> Just $ OutputStubs

--     "defs"          -> Just $ OutputDefinitions

--     _               -> Nothing


-- parseConfig :: Arguments -> IO Config
-- parseConfig args = do
--   let format = getArgWithDefault args "jsons-listed" (longOption "format")
--   oformat <- case parseOutputFormat format of
--     Nothing -> error ("Could not recognize output-format: '" ++ format ++ "'")
--     Just fs -> return fs

--   classnames <- readClassNames

--   stubs :: HierarchyStubs <-
--     case getArg args (longOption "stub") of
--       Just fn -> do
--         x <- decode <$> BS.readFile fn
--         case x of
--           Just x -> do
--             hPutStrLn stderr "Loaded stub-file"
--             return x
--           Nothing -> fail $ "Could not decode " ++ fn
--       Nothing -> return $ mempty

--   return $ Config
--     { _cfgClassPath =
--         case concatMap splitClassPath $ getAllArgs args (longOption "cp") of
--           [] -> ["."]
--           as -> as
--     , _cfgUseStdlib = isPresent args (longOption "stdlib")
--     , _cfgJre = getArg args (longOption "jre")
--     , _cfgClassNames = classnames
--     , _cfgFormat = oformat
--     , _cfgStubs = stubs
--     , _cfgKeepAttributes = not (isPresent args (longOption "remove-attr"))
--     }

--   where
--     classnames' = getAllArgs args $ argument "classname"
--     readClassNames = do
--       names <-
--         if isPresent args (command "-")
--         then do (classnames' ++) . lines <$> getContents
--         else return classnames'
--       return . map strCls $ names

-- main :: IO ()
-- main = do
--   args' <- parseArgs patterns <$> getArgs
--   case args' of
--     Left msg -> do
--       hPrint stderr msg
--       exitWithUsage patterns
--     Right args -> do
--       if isPresent args (longOption "help")
--         then do
--           exitWithUsage patterns
--         else do
--           cfg <- parseConfig args
--           decompile cfg

-- -- | Decompile and print a classfile to stdout
-- decompile :: Config -> IO ()
-- decompile cfg = do
--   r <- preload =<< createClassLoader cfg
--   classnames <-
--     case cfg ^. cfgClassNames of
--       [] -> map fst <$> classes r
--       a  -> return a
--   let
--     readerOpt = ( defaultFromReader r ) { keepAttributes = _cfgKeepAttributes cfg }
--     onEachClass doeach dofinal = do
--       (fails,succs) <- partitionEithers <$> forM classnames (
--         \cn -> force (over _Left (cn,)) <$> doeach cn
--         )
--       forM_ fails (hPrint stderr)
--       dofinal succs
--   case cfg ^. cfgFormat of
--     OutputJSON dto ->
--       case dto of
--         FullDTO ->
--           onEachClass (flip readClass readerOpt) $ BS.putStrLn . encode
--         ListedDTO ->
--           onEachClass (readClassOverview readerOpt) $ BS.putStrLn . encode
--         CountDTO ->
--           onEachClass (readClassCount readerOpt) $ BS.putStrLn . encode
--     OutputJSONs dto ->
--       case dto of
--         FullDTO ->
--           onEachClass (flip readClass readerOpt >=> encode') (const $ return ())
--         ListedDTO ->
--           onEachClass (readClassOverview readerOpt >=> encode') (const $ return ())
--         CountDTO ->
--           onEachClass (readClassCount readerOpt >=> encode') (const $ return ())
--     OutputCSV -> do
--       putStrLn "class,sha256,size,super,interfaces,fields,methods"
--       onEachClass
--         (\cn -> do
--           cc' <- readClassCount readerOpt cn
--           case cc' of
--             Right cc ->
--               Right <$> (putStrLn . List.intercalate "," $
--                 [ Text.unpack (cc^.ccName.fullyQualifiedName)
--                 , Text.unpack (cc^.ccSha256)
--                 , show (cc^.ccSize)
--                 , Text.unpack (fromMaybe "java/lang/Object" (cc^?ccSuper._Just.fullyQualifiedName))
--                 , show (cc^.ccInterfaces)
--                 , show (cc^.ccFields)
--                 , show (cc^.ccMethods)
--                 ])
--             Left msg -> return $ Left msg
--         ) (const $ return ())
--     OutputDot GTClass gf -> do
--       (graph, _) <- flip runClassPoolTWithReader readerOpt $ \_ -> do
--         mkClassGraph
--       putStrLn $ graphToDot' graph (Text.unpack . view fullyQualifiedName) (const "")
--     OutputDot GTCall gf -> do
--       ((missing, graph), _) <- flip runClassPoolTWithReader readerOpt $ \_ -> do
--         (_, hry) <- getHierarchyWithStubs (cfg ^. cfgStubs)
--         mkCallGraph hry
--       putStrLn $ graphToDot' graph (Text.unpack . view (inClassToText methodIdToText)) (const "")
--       forM_ missing $ \mn -> do
--         hPutStrLn stderr $ "WARN: missing " ++ Text.unpack (mn ^. inClassToText methodIdToText)
--     OutputDot GTHierarchy gf -> do
--       ((missing, graph), _) <- flip runClassPoolTWithReader readerOpt $ \_ -> do
--         (missing, hry) <- getHierarchyWithStubs (cfg ^. cfgStubs)
--         return (missing, hry ^. hryGraph)
--       putStrLn $ graphToDot' graph (Text.unpack . view fullyQualifiedName) show
--       forM_ missing $ \cn -> do
--         hPutStrLn stderr $ "WARN: missing " ++ show cn
--     OutputStubs -> do
--       (results, _) <- flip runClassPoolTWithReader readerOpt $ \_ -> do
--         expandStubs (cfg ^. cfgStubs)
--       BS.putStrLn $ encode results
--     -- OutputDefinitions -> do
--     --   (results, _) <- flip runClassPoolTWithReader readerOpt $ \_ -> do
--     --     (_, hry) <- getHierarchyWithStubs (cfg ^. cfgStubs)
--     --     -- methods <- classnames ^!!
--     --     --   (ifolded . selfIndex <. pool . ifolded . classMethods . ifolded . asIndex)
--     --     --   . withIndex . to (uncurry inClass)
--     --     -- return [ (m ^. inClassToText methodIdToText, callSites hry m ^.. folded . inClassToText methodIdToText) | m <- methods ]
--     --   BS.putStrLn $ encode results

--   where
--      encode' :: ToJSON e => Either a e -> IO (Either a ())
--      encode' =
--        either (return . Left) (fmap Right . BS.putStrLn . encode)

-- readClassOverview ::
--   ClassReader r
--   => ReaderOptions r
--   -> ClassName
--   -> IO (Either ClassReadError ClassOverview)
-- readClassOverview readerOpt cn = do
--   bytes <- getClassBytes (classReader readerOpt) cn
--   return $ do
--     b <- bytes
--     clsf <- readClassBytes readerOpt b
--     let
--       cls = convertClass clsf
--       (hsh, lth) = hashlazyAndLength b
--     return $! ClassOverview
--       (cls^.className)
--       (Text.decodeUtf8 . B16.encode $ hsh)
--       (fromIntegral lth)
--       (cls^.classSuper)
--       (cls^..classInterfaces.folded)
--       (cls^..classFields.ifolded.asIndex)
--       (cls^..classMethods.ifolded.asIndex)

-- readClassCount ::
--   ClassReader m
--   => ReaderOptions m
--   -> ClassName
--   -> IO (Either ClassReadError ClassCount)
-- readClassCount readerOpt cn = do
--   co' <- readClassOverview readerOpt cn
--   return $ do
--     co <- co'
--     return $! ClassCount
--       (co^.coName)
--       (Text.take 7 $ co^.coSha256)
--       (co^.coSize)
--       (co^.coSuper)
--       (length $ co^.coInterfaces)
--       (length $ co^.coFields)
--       (length $ co^.coMethods)

