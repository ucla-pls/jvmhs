{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
module JavaQ where

-- base
import           Control.Monad
import qualified Data.List                    as List
import qualified Data.Maybe                   as Maybe
import           Data.Word
import           System.Environment
import           System.IO

import Debug.Trace as T

-- fgl
import qualified Data.Graph.Inductive         as FGL

-- unordered
import qualified Data.HashSet                 as Set

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BL

-- aeson
import           Data.Aeson
import           Data.Aeson.Encoding.Internal (encodingToLazyByteString)
import           Data.Aeson.TH

-- mtl
import           Control.Monad.Reader
import           Control.Monad.Writer.Strict
import           Control.Monad.State

-- text
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text

-- filepath
import           System.FilePath

-- vector
import qualified Data.Vector                  as V

-- optparse-applicative
import           Options.Applicative

-- ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as D

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- jvmhs
import           Jvmhs
import           Jvmhs.Data.Code

-- cryptohash-sha256
import           Crypto.Hash.SHA256           as SHA256

-- hexstring
import           Data.HexString

-- javaq
import           JavaQ.CHA

data OutputFormat
  = Stream (StreamFunction (ReaderT Config IO))
  | Aggregate ([ClassPoolReadError] -> ClassPoolT (ReaderT Config IO) ())
  | Folding (FoldFunction (ReaderT Config IO))
  | Group [Format]

data FoldFunction m
  = forall r. (ToJSON r, FromJSON r, Monoid r) => MonoidalFoldClass (Class -> m r) 
  | forall r. (ToJSON r, FromJSON r)           => FoldClass r (r -> Class -> m r) 

data StreamFunction m
  = StreamContainer ( (ClassName, ClassContainer) -> m () )
  | StreamClassName ( ClassName -> m () )
  | StreamClass ( Class -> m () )

data Format = Format
  { formatName        :: Text.Text
  , formatDescription :: D.Doc
  , formatType        :: OutputFormat
  }
instance Show Format where
  show x = "Format { formatName = " ++ show (formatName x) ++ " , ...}"

-- | The config file dictates the execution of the program
data Config = Config
  { _cfgClassPath      :: ClassPath
  , _cfgInitial        :: Maybe FilePath
  , _cfgJre            :: FilePath
  , _cfgUseStdlib      :: Bool
  , _cfgComputeClosure :: Bool
  , _cfgFast           :: Bool
  , _cfgFormat         :: Format
  -- , _cfgStubs          :: HierarchyStubs
  , _cfgClassNames     :: [ ClassName ]
  } deriving (Show)

makeLenses ''Config

data ClassMetric = ClassMetric
  { cmName         :: !ClassName
  , cmSize         :: !Word64
  , cmSha256       :: !HexString
  , cmMethods      :: !Int
  , cmFields       :: !Int
  , cmInstructions :: !Int
  } deriving (Show, Eq)


$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 2} ''ClassMetric)


getConfigParser :: [Format] -> IO (Parser (IO Config))
getConfigParser formats' = do
  let format = head formats'

  mclasspath <- lookupEnv "CLASSPATH"

  jre <-
    lookupEnv "JAVA_HOME" >>= \case
      Just javaHome -> return $ javaHome </> "jre"
      Nothing -> guessJre

  return $
    readConfig

    <$> option str
    ( long "cp"
      <> help "The classpath to search for classes. Defaults to $CLASSPATH."
      <> metavar "CLASSPATH"
      <> (Maybe.maybe mempty value $ mclasspath)
      <> showDefault
    )

    <*> ( Just <$> option str
          ( long "initial"
            <> help "Initial values for a fold"
            <> metavar "INITIAL"
          ) <|> pure Nothing )

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

    <*> option (maybeReader $ findFormat formats' . Text.splitOn "-" . Text.pack)
    ( long "format" <> short 'F'
      <> help "The output format, see Formats."
      <> metavar "FORMAT"
      <> value (format)
      <> showDefault
    )

    <*> many
    ( argument str
      ( metavar "CLASS .."
      <> help "Classes to investigate, if none present all will be listed."
      )
    )
  where
    readConfig classpath initial jre useStdlib computeClosure fast fmt classNames' = do
      return $
        Config
        (splitClassPath classpath)
        initial
        jre
        useStdlib
        computeClosure
        fast
        fmt
        classNames'

    findFormat fmts formatlist = do
      (a, rest) <- uncons formatlist
      format <- List.find ((a ==) . formatName) fmts

      case (rest, formatType format) of
        ([], Group (fmt:_)) ->
          return fmt
        ([], _) ->
          return format
        (more, Group fmts') ->
          findFormat fmts' more
        _ -> Nothing


footerFromFormats :: [Format] -> D.Doc
footerFromFormats fts =
  D.nest 2 $
    D.text "Formats:"
    D.<$> "Here is a list of all formats that can be used with the"
    D.<+> "with the --format option."
    D.<$> D.empty
    D.<$> joinFormats fts (formatFormatter "")
  where
    joinFormats fs fn =
      D.vcat . map ((<> D.line) . fn) $ fs

    formatFormatter prefix (Format name desc type_) =
      D.nest 2 $
      let
        nameDoc = prefix D.<> doc name
        title = D.green (nameDoc D.<> D.colon)
      in case type_ of
        Stream _ ->
          title D.<+> D.parens "stream"
          D.<$> desc
        Aggregate _ ->
          title D.<+> D.parens "aggregate"
          D.<$> desc
        Folding _ ->
          title D.<+> D.parens "fold"
          D.<$> desc
        Group fmts ->
          let prefix' = prefix D.<> doc name D.<> "-"
          in
            title D.<+> D.parens ("default:" D.<+> (prefix' D.<> (doc . formatName . head $ fmts)))
            D.<$> desc
            D.<$> D.empty
            D.<$> joinFormats fmts (formatFormatter (prefix D.<> doc name D.<> "-"))

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
    inClasspool (streamAll sfn)

  Folding ff ->
    case ff of
      MonoidalFoldClass folder -> do
        initial <- view cfgInitial >>= \case
          Just fp -> do
            x <- liftIO (decode <$> BL.readFile fp)
            return $ Maybe.fromMaybe mempty x
          Nothing ->
            return mempty
        r <- execWriterT $ inClasspool $ do
          streamClasses $ \cls -> do
            a <- lift $ folder cls
            tell a
        liftIO . BL.putStrLn . encode $ initial <> r
      FoldClass def foldf -> do
        initial <- view cfgInitial >>= \case
          Just fp -> do
            x <- liftIO (decode <$> BL.readFile fp)
            return $ Maybe.fromMaybe def x
          Nothing ->
            return def
        T.traceM $ "initial: \n" ++ (BL.unpack $ encode initial) 
        r <- flip execStateT initial $ inClasspool $ do
          streamClasses $ \cls -> do
            s <- get
            a <- lift $ foldf s cls
            traceM $ "for class: " ++ show (cls^.className) ++ " state after: \n" ++ BL.unpack (encode a) ++ "\n"
            put a
        liftIO . BL.putStrLn . encode $ r

  Aggregate m -> do
    preloaded' <- liftIO $ preload classloader
    opts <- ( \isFast ->
      (defaultFromReader preloaded') { keepAttributes = not isFast }
      ) <$> view cfgFast
    void $ runClassPoolTWithReader m opts

  Group ( f:_ ) ->
    runFormat classloader (formatType f)

  Group [] ->
    error "Bad Format"

  where
    inClasspool :: forall m. (MonadReader Config m, MonadIO m) => (forall r. ClassReader r => CachedClassPoolT r m ()) -> m ()
    inClasspool dothis = do
      preloaded' <- liftIO ( preload classloader )
      isFast <- view $ cfgFast
      let opts = (defaultFromReader preloaded') { keepAttributes = not isFast }
      void . flip runCachedClassPoolT opts $ do
        view cfgClassNames >>= \case
          [] ->
            dothis
          classNames' -> do
            let classSet = Set.fromList classNames'

            view cfgComputeClosure >>= \case
              True -> do
                void . computeClassClosureM classSet $ \(_, clss) ->
                  cplocal $ do
                    restrictTo (Set.fromList $ toListOf (folded.className) clss)

              False -> do
                restrictTo classSet
                dothis

formats :: [Format]
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
  , Format "json" "Output each class as json" jsons
  , Format "classgraph" "Inspect the classpath as a class-graph"
  $ Group
    [ Format "metric" "Just compute the metrics"
    . Aggregate $ \err -> do
      grph <- mkClassGraph
      let nonodes = grph^.innerGraph.to FGL.order
      let
        noscc = List.length $ partition' grph
        meanOf x =
          List.sort (toListOf x grph) List.!! (nonodes `div` 2)
      liftIO . BL.putStrLn . encodingToLazyByteString . pairs
        $ "errors" .= List.length err
        <> "nodes" .= nonodes
        <> "edges" .= (grph^.innerGraph.to FGL.size)
        <> "scc" .= noscc
        <> "out_degree" .= meanOf (graphContexts.to FGL.outdeg')
        <> "in_degree" .= meanOf (graphContexts.to FGL.indeg')
    ]
  , Format "interfaces" "Output all interfaces with classes that implement them" interfaces
  ]

interfaces :: OutputFormat
interfaces = Group
  [ Format "itfcs" "<itfc>: <implementing classes>(, <impl classes>)*"
  . Folding
  $ FoldClass emptyCHA (\cha cls -> return (addNode cha cls))
  ]

jsons :: OutputFormat
jsons = Group
  [ Format "full" "Full output of the class"
    . Stream . StreamClass $ liftIO . BL.putStrLn . encode
  , Format "metric" "Contains only the overall metrics of the class"
    . Stream . StreamContainer $ metrics
  ]
  where
    metrics (cn, lo) = do
      ebts <- liftIO $ getClassBytes lo cn
      let readByte bts = (bts,) <$> readClassFile' True bts
      case ebts >>= readByte of
        Left err ->
          liftIO . Text.hPutStrLn stderr
          $ "Error in " <> cn^.fullyQualifiedName <> ": " <> Text.pack (show err)
        Right (bts, clsf) -> liftIO $ do
          let
            (hsh, ln) = SHA256.hashlazyAndLength bts
            cls = convertClass clsf
          BL.putStrLn . encode $ ClassMetric
            { cmName = cn
            , cmSize = ln
            , cmSha256 = fromBytes hsh
            , cmFields = cls ^. classFieldList.to length
            , cmMethods = cls ^. classMethodList.to length
            , cmInstructions =
                sumOf (classMethods.folded.methodCode._Just.codeByteCode.to V.length) cls
            }

streamAll ::
  (ClassReader r, MonadIO m)
  => StreamFunction m
  -> CachedClassPoolT r m ()
streamAll = \case
  StreamClassName fn -> do
    mapM_ (lift . fn) =<< allClassNames
  StreamContainer fn -> do
    r <- classReader <$> CachedClassPoolT ask
    c <- liftIO (classes r)
    mapM_ (lift . fn) c
  StreamClass fn -> do
    streamClasses fn

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


doc :: Text.Text -> D.Doc
doc = D.text . Text.unpack

