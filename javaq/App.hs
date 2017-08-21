{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Control.Lens hiding (argument)

import System.Environment (getArgs)
import System.Console.Docopt

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BL

import Language.JVM.Hierarchy
import Language.JVM.ClassName (fromDotStr)
import Language.JVM.ClassLoader
import qualified Language.JVM.Method as Method
import qualified Language.JVM.Class as Class

import Data.Aeson

import qualified Data.Set as S

patterns :: Docopt
patterns = [docopt|
javaq version one

Usage:
  javaq list-class [options] <class>
  javaq list-indirect-methods [options] <class>

Options:
  --classpath=<classpath>  The classpath
  --only-classpath         Do not search the stdlib
  --jre=<jre>         Specify the jre folder
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

  when (args `isPresent` (command "list-class")) $ do
    cn <- getArgOrExitWith patterns args $ argument "class"

    res <- runHierarchy classloader $ do
      getClass (fromDotStr cn)

    BL.putStrLn $ encode res

  when (args `isPresent` (command "list-indirect-methods")) $ do
    cn <- fromDotStr <$> getArgOrExitWith patterns args (argument "class")

    res <- runHierarchy classloader $ do
      inter <- S.toList <$> indirectInterfaces cn
      concat <$> mapM (hview $ Class.methods . to (map (Method.desc cn))) inter

    mapM_ TIO.putStrLn res
