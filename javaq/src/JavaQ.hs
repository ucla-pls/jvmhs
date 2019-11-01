{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
import           JavaQ.Command.ClassHierarchyAnalysis

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
    , chaCmd
    ]
  runConfig config

runConfig :: Config -> IO ()
runConfig = runReaderT $ do
  join (runCommand <$> createClassLoader <*> view cfgCommand)

runCommandConfig :: ReaderT CommandConfig IO a -> ReaderT Config IO a
runCommandConfig m = do
  cmdCfg <- view cfgCommandConfig
  liftIO $ runReaderT m cmdCfg

runCommand :: ClassLoader -> Command -> ReaderT Config IO ()
runCommand classloader (Command _ (fmt :: Format a) tp) = do
  preludeFormat fmt
  runCommandType tp
  where
  runCommandType = \case
    Stream gran fn -> do
      fn' <- runCommandConfig fn
      case gran of
        ClassNames -> inClasspool $ do
          r <- classReader <$> CachedClassPoolT ask
          liftIO $ do
            mapM_ (applyFormat fmt . fn') =<< classes r

        ClassFiles -> inClasspool $ do
          r <- classReader <$> CachedClassPoolT ask
          liftIO $ do
            clss <- classes r
            forM_ clss $ \(cn, cc) -> do
              getClassBytes r cn >>= \case
                Right cls -> applyFormat fmt $ fn' (cn, cc, cls)
                Left msg -> hPrint stderr msg

        Classes ->
          inClasspool $ streamClasses (liftIO . applyFormat fmt . fn')

        Methods -> inClasspool $ do
          streamClasses $ \cls -> liftIO $ do
          forMOf_ (classMethods.folded) cls $ \m ->
            applyFormat fmt (fn' (cls^.className, m))

        Fields -> inClasspool $ do
          streamClasses $ \cls -> liftIO $ do
            forMOf_ (classFields.folded) cls $ \f ->
              applyFormat fmt (fn' (cls^.className, f))

    Accumulator Classes fn initial acc -> do
      preludeFormat fmt
      fn' <- runCommandConfig fn
      (r, _) <- flip execStateT (initial, 0 :: Int) $ inClasspool $ do
        r <- classReader <$> CachedClassPoolT ask
        count <- length <$> liftIO (classes r)
        streamClasses $ \cls -> do
          modify' (\(s, n) -> (acc s (fn' cls), (n+1)))
          n <- gets snd
          when (n `mod` 100 == 0) . liftIO $ do
            hPutStr stderr $ show n ++ "/" ++ show count ++ "\r"
            hFlush stderr
      liftIO $ applyFormat fmt r

    Accumulator _ _ _ _ -> return ()


    Algorithm fn -> do
      inClasspool $ do
        a <- fn
        liftIO $ applyFormat fmt a

  preludeFormat :: Format a -> ReaderT Config IO ()
  preludeFormat = \case
    Csv h _ ->
      liftIO . BL.putStr $ Csv.encode [h]
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
