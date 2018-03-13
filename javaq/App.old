{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Lens hiding (argument)

import Debug.Trace

import System.Environment (getArgs)
import System.Console.Docopt

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.JVM.Hierarchy
import Language.JVM.ClassName (fromDotStr, ClassName(..), toText)
import Language.JVM.ClassLoader
import qualified Language.JVM.Method as Method
import qualified Language.JVM.Class as Class

import Data.Aeson

import qualified Data.Set as S
import qualified Data.Map as M

patterns :: Docopt
patterns = [docopt|
javaq version one

Usage:
  javaq decompile [options] <class>
  javaq list-classes [options]
  javaq list-indirect-methods [options] [<class>...]
  javaq list-methods [options] [<class>...]

Options:
  --classpath=<classpath>  The classpath
  --only-classpath         Do not search the stdlib
  --jre=<jre>              Specify the jre folder
  --no-preload             Disable preloading of jars
|]

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  let classpath = case getArgWithDefault args "" (longOption "classpath") of
        "" -> []
        s  -> Prelude.map T.unpack . T.splitOn ";" . T.pack $ s

  classloader <- if args `isPresent` (longOption "only-classpath")
    then return $ ClassLoader [] [] classpath
    else case args `getArg` (longOption "jre") of
           Just folder -> fromJreFolder classpath folder
           Nothing -> fromClassPath classpath

  when (args `isPresent` (command "list-classes")) $ do
    preloader <- preloadClasses classloader
    mapM_ (TIO.putStrLn . toText) $  M.keys (classMap preloader)

  interp <- if args `isPresent` (longOption "no-preload")
    then return $ flip loadClass classloader
    else do
       preloader <- preloadClasses classloader
       return $ flip loadClassFromPreloader preloader

  when (args `isPresent` (command "decompile")) $ do
    cn <- getArgOrExitWith patterns args $ argument "class"

    res <- runHierarchy interp $ do
      getClass (fromDotStr cn)

    BL.putStrLn $ encode res

  when (args `isPresent` (command "list-indirect-methods")) $ do
    cns <- case getAllArgs args (argument "class") of
      [] -> map (ClassName . TL.toStrict) . TL.lines <$> TLIO.getContents
      xs -> return $ map fromDotStr xs

    runHierarchy interp $ do
      forM_ cns $ \cn -> do
        traceM $ "Loaded " ++ show cn
        inter <- S.toList <$> indirectInterfaces cn
        methods <- concat <$> mapM (hview $ Class.methods . to (map (Method.desc cn))) inter
        forM_ methods putText


  when (args `isPresent` (command "list-methods")) $ do
    cns <- case getAllArgs args (argument "class") of
      [] -> map (ClassName . TL.toStrict) . TL.lines <$> TLIO.getContents
      xs -> return $ map fromDotStr xs

    runHierarchy interp $ do
      forM_ cns $ \cn -> do
        traceM $ "Loaded " ++ show cn
        methods <- hview (Class.methods . to (map $ Method.desc cn)) cn
        forM_ methods putText
