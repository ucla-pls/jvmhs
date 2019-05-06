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
import           Data.Foldable
import           System.IO

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BL

-- unordered
import qualified Data.HashSet                 as Set

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- aeson
import qualified Data.Aeson as Json

-- mtl
import           Control.Monad.Reader
import           Control.Monad.State

-- text
import qualified Data.Text.IO                 as Text

-- cassava
import qualified Data.Csv                     as Csv

-- jvmhs
import           Jvmhs

-- javaq
import           JavaQ.Config
import           JavaQ.Command
import           JavaQ.Command.Base
import           JavaQ.Command.ClassMetric
import           JavaQ.Command.MethodMetric
import           JavaQ.Command.Hierarchy

main :: IO ()
main = do
  config <- getConfig
    [ listClassesCmd
    , listMethodsCmd
    , listFieldsCmd
    , containersCmd
    , decompileCmd
    , classmetricsCmd
    , methodmetricCmd
    , hierarchyCmd
    ]
  runConfig config

-- run :: ReaderT Config IO ()
-- run = do
--   classloader <- createClassLoader
--   format <- view (cfgFormat . to formatType)
--   runFormat classloader format

-- runFormat ::
--   (ClassReader r)
--   => r
--   -> OutputFormat
--   -> ReaderT Config IO ()
-- runFormat classloader = \case
--   Stream sfn -> do
--     inClasspool (streamAll sfn)

--   Folding ff ->
--     case ff of
--       MonoidalFoldlass folder -> do
--         initial <- view cfgInitial >>= \case
--           Just fp -> do
--             x <- liftIO (decode <$> BL.readFile fp)
--             return $ Maybe.fromMaybe mempty x
--           Nothing ->
--             return mempty
--         r <- execWriterT $ inClasspool $ do
--           streamClasses $ \cls -> do
--             a <- lift $ folder cls
--             tell a
--         liftIO . BL.putStrLn . encode $ initial <> r
--       FoldClass def foldf -> do
--         initial <- view cfgInitial >>= \case
--           Just fp -> do
--             x <- liftIO (decode <$> BL.readFile fp)
--             return $ Maybe.fromMaybe def x
--           Nothing ->
--             return def
--         T.traceM $ "initial: \n" ++ (BL.unpack $ encode initial)
--         r <- flip execStateT initial $ inClasspool $ do
--           streamClasses $ \cls -> do
--             s <- get
--             a <- lift $ foldf s cls
--             traceM $ "for class: " ++ show (cls^.className) ++ " state after: \n" ++ BL.unpack (encode a) ++ "\n"
--             put a
--         liftIO . BL.putStrLn . encode $ r

--   Aggregate m -> do
--     preloaded' <- liftIO $ preload classloader
--     opts <- ( \isFast ->
--       (defaultFromReader preloaded') { keepAttributes = not isFast }
--       ) <$> view cfgFast
--     void $ runClassPoolTWithReader m opts

--   Group ( f:_ ) ->
--     runFormat classloader (formatType f)

--   Group [] ->
--     error "Bad Format"

--   where
--     inClasspool :: forall m. (MonadReader Config m, MonadIO m) => (forall r. ClassReader r => CachedClassPoolT r m ()) -> m ()
--     inClasspool dothis = do
--       preloaded' <- liftIO ( preload classloader )
--       isFast <- view $ cfgFast
--       let opts = (defaultFromReader preloaded') { keepAttributes = not isFast }
--       void . flip runCachedClassPoolT opts $ do
--         view cfgClassNames >>= \case
--           [] ->
--             dothis
--           classNames' -> do
--             let classSet = Set.fromList classNames'

--             view cfgComputeClosure >>= \case
--               True -> do
--                 void . computeClassClosureM classSet $ \(_, clss) ->
--                   cplocal $ do
--                     restrictTo (Set.fromList $ toListOf (folded.className) clss)

--               False -> do
--                 restrictTo classSet
--                 dothis

-- formats :: [Format]
-- formats =
--   [ Format "list" "List classes available on the classpath"
--     $ Stream . StreamClassName $ \cn -> liftIO $ do
--       Text.putStrLn (cn ^. fullyQualifiedName)
--   , Format "containers"
--     ( "List classes and their containers, outputs in a tap-separated fashion."
--       D.</> "To get a count of classes per container, try:"
--       D.<$> D.indent 4 "> javaq --stdlib --cp /dev/null --format containers | cut -f 2 | sort | uniq -c"
--     )
--     $ Stream . StreamContainer $ \(cn, lo) -> liftIO $ do
--       Text.putStr (cn ^. fullyQualifiedName)
--       Text.putStr "\t"
--       case lo of
--         CCFolder (CFolder fp)           -> putStrLn fp
--         CCJar (CJar fp _)               -> putStrLn fp
--         CCEntry (CEntry (CJar fp _, _)) -> putStrLn fp
--   , Format "json" "Output each class as json" jsons
--   , Format "classgraph" "Inspect the classpath as a class-graph"
--   $ Group
--     [ Format "metric" "Just compute the metrics"
--     . Aggregate $ \err -> do
--       grph <- mkClassGraph
--       let nonodes = grph^.innerGraph.to FGL.order
--       let
--         noscc = List.length $ partition' grph
--         meanOf x =
--           List.sort (toListOf x grph) List.!! (nonodes `div` 2)
--       liftIO . BL.putStrLn . encodingToLazyByteString . pairs
--         $ "errors" .= List.length err
--         <> "nodes" .= nonodes
--         <> "edges" .= (grph^.innerGraph.to FGL.size)
--         <> "scc" .= noscc
  --         <> "out_degree" .= meanOf (graphContexts.to FGL.outdeg')
--         <> "in_degree" .= meanOf (graphContexts.to FGL.indeg')
--     ]
--   , Format "hierarchy" "Outputs the class relation relation" hierarchy
--   ]

-- hierarchy :: OutputFormat
-- hierarchy = Group
--   [ Format "hierarchy" "Includes superclasses, extendedBy, implementedBy, implements"
--   . Folding
--   $ FoldClass emptyHR (\hr cls -> return (addNode hr cls))
--   ]

-- jsons :: OutputFormat
-- jsons = Group
--   [ Format "full" "Full output of the class"
--     . Stream . StreamClass $ liftIO . BL.putStrLn . encode
--   , Format "metric" "Contains only the overall metrics of the class"
--     . Stream . StreamContainer $ metrics
--   ]
--   where
--     metrics (cn, lo) = do
--       ebts <- liftIO $ getClassBytes lo cn
--       let readByte bts = (bts,) <$> readClassFile' True bts
--       case ebts >>= readByte of
--         Left err ->
--           liftIO . Text.hPutStrLn stderr
--           $ "Error in " <> cn^.fullyQualifiedName <> ": " <> Text.pack (show err)
--         Right (bts, clsf) -> liftIO $ do
--           let
--             (hsh, ln) = SHA256.hashlazyAndLength bts
--             cls = convertClass clsf
--           BL.putStrLn . encode $ ClassMetric
--             { cmName = cn
--             , cmSize = ln
--             , cmSha256 = fromBytes hsh
--             , cmFields = cls ^. classFieldList.to length
--             , cmMethods = cls ^. classMethodList.to length
--             , cmInstructions =
--                 sumOf (classMethods.folded.methodCode._Just.codeByteCode.to V.length) cls
--             }

-- streamAll ::
--   (ClassReader r, MonadIO m)
--   => StreamFunction m
--   -> CachedClassPoolT r m ()
-- streamAll = \case
--   StreamClassName fn -> do
--     mapM_ (lift . fn) =<< allClassNames
--   StreamContainer fn -> do
--     r <- classReader <$> CachedClassPoolT ask
--     c <- liftIO (classes r)
--     mapM_ (lift . fn) c
--   StreamClass fn -> do
--     streamClasses fn



-- doc :: Text.Text -> D.Doc
-- doc = D.text . Text.unpack
runConfig :: Config -> IO ()
runConfig = runReaderT $ do
  join (runCommand <$> createClassLoader <*> view cfgCommand)

runCommand :: ClassLoader -> Command -> ReaderT Config IO ()
runCommand classloader (Command _ fmt tp) = do
  case tp of
    Stream (ClassNames fn) -> inClasspool $ do
      r <- classReader <$> CachedClassPoolT ask
      liftIO $ do
        preludeFormat fmt
        mapM_ (applyFormat fmt . uncurry fn) =<< classes r

    Stream (ClassFiles fn) -> inClasspool $ do
      r <- classReader <$> CachedClassPoolT ask
      liftIO $ do
        clss <- classes r
        preludeFormat fmt
        forM_ clss $ \(cn, cc) -> do
          getClassBytes r cn >>= \case
            Right cls -> applyFormat fmt $ fn cn cc cls
            Left msg -> hPrint stderr msg

    Stream (Classes fn) -> inClasspool $  do
      liftIO $ preludeFormat fmt
      streamClasses (liftIO . applyFormat fmt . fn)

    Stream (Methods fn) -> inClasspool $ do
      liftIO $ preludeFormat fmt
      streamClasses $ \cls -> liftIO $ do
        forMOf_ (classMethodList.folded) cls $ \m ->
          applyFormat fmt (fn (cls^.className) m)

    Stream (Fields fn) -> inClasspool $ do
      liftIO $ preludeFormat fmt
      streamClasses $ \cls -> liftIO $ do
        forMOf_ (classFieldList.folded) cls $ \f ->
          applyFormat fmt (fn (cls^.className) f)

    Accumulator (Classes fn) initial acc -> do
      liftIO $ preludeFormat fmt
      (r, _) <- flip execStateT (initial, 0 :: Int) $ inClasspool $ do
        r <- classReader <$> CachedClassPoolT ask
        count <- length <$> liftIO (classes r)
        streamClasses $ \cls -> do
          modify' (\(s, n) -> (acc s (fn cls), (n+1)))
          n <- gets snd
          when (n `mod` 100 == 0) . liftIO $ do
            hPutStr stderr $ show n ++ "/" ++ show count ++ "\r"
            hFlush stderr
      liftIO $ applyFormat fmt r

    Accumulator _ _ _ -> return ()


    Algorithm fn -> do
      inClasspool $ do
        a <- fn
        liftIO $ applyFormat fmt a

  where
    preludeFormat = \case
      Csv h _ ->
        BL.putStr $ Csv.encode [h]
      _ -> return ()

    applyFormat = \case
      Txt fn -> Text.putStrLn . fn
      Csv _ fn ->
        BL.putStr . Csv.encode . fn
      Json fn ->
        BL.putStrLn . Json.encode . fn

    inClasspool ::
      forall m. (MonadReader Config m, MonadIO m)
      => (forall r. ClassReader r => CachedClassPoolT r m ())
      -> m ()
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
