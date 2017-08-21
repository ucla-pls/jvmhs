{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- This module contains the virtual class loader

module Language.JVM.ClassLoader
  ( ClassLoader (..)
  , loadClass
  , fromClassPath
  , fromJreFolder
  ) where

import           Language.JVM.Class
import           Language.JVM.ClassName
import           Language.JVM.Method
import           System.Directory
import           System.FilePath
import           System.Process

import Control.Lens

import Control.Monad (forM_)

import qualified Data.Vector as V

import Data.Maybe (catMaybes)

import           Debug.Trace

import qualified Data.ByteString.Lazy   as BL

import           Codec.Archive.Zip

import qualified Data.Map.Lazy          as Map

data ClassLoader = ClassLoader
  { lib       :: [ FilePath ]
  , ext       :: [ FilePath ]
  , classpath :: [ FilePath ]
  } deriving (Show, Eq)

fromClassPath :: [ FilePath ] -> IO ClassLoader
fromClassPath fps = do
  java <- readProcess "which" ["java"] ""
  fromJreFolder fps $ takeDirectory (takeDirectory java) </> "jre"

-- creates a ClassLoader from a classpath
fromJreFolder :: [ FilePath ] -> FilePath -> IO ClassLoader
fromJreFolder clspath jre =
  ClassLoader
    <$> (jarsFromFolder $ jre </> "lib")
    <*> (jarsFromFolder $ jre </> "lib/ext")
    <*> pure clspath

-- Returns the paths in the order they should be checked for classes
paths :: ClassLoader -> [ FilePath ]
paths cl =
  lib cl ++ ext cl ++ classpath cl

loadClass :: ClassName -> ClassLoader -> IO [(FilePath, Either String Class)]
loadClass cn cl = do
  x <- mapM tryload $ paths cl
  return $ catMaybes x
  where
    tryload path = do
      bc <- read path
      return $ case bc of
        Just (path, bs) ->
          Just (path, decodeClassOrFail bs)
        Nothing ->
          Nothing

    read path | isJar path = do
       arch <- readZipFile path
       return $ (path,) . fromEntry <$>
         findEntryByPath (pathOfClass "" cn) arch

    read path = do
      test <- doesFileExist path
      if test
        then do
          bc <- BL.readFile path
          return $ Just (path, bc)
        else return Nothing

findClassesInFolder :: FilePath -> IO [ ClassName ]
findClassesInFolder fp = do
  items <- folderContents fp
  concat <$> sequence (map go items)
  where
    go fp = do
     guard <- doesDirectoryExist fp
     if guard
       then findClassesInFolder fp
       else do
         let (filepath, ext) = splitExtension fp
         if ext == ".class"
           then return [ fromStr filepath ]
           else return []

findClassesInFile :: FilePath -> [ ClassName ]
findClassesInFile fp = do
  let (filepath, ext) = splitExtension fp
  case ext of
    ".class" -> return $ fromStr filepath
    _        -> []

findClassesInZip :: FilePath -> IO [ ClassName ]
findClassesInZip fp = do
  concatMap findClassesInFile <$> readZipFileContent fp

jarsFromFolder :: FilePath -> IO [ FilePath ]
jarsFromFolder fp =
  filter isJar <$> folderContents fp

-- Helpers

readZipFile :: FilePath -> IO Archive
readZipFile fp = do
  toArchive <$> BL.readFile fp

readZipFileContent :: FilePath -> IO [ FilePath ]
readZipFileContent fp = do
  filesInArchive <$> readZipFile fp

-- Folders
folderContents :: FilePath -> IO [ FilePath ]
folderContents fp =
  map (fp </>) <$> listDirectory fp

recursiveContents :: FilePath -> IO [ FilePath ]
recursiveContents fp = do
  test <- doesDirectoryExist fp
  (fp:) <$> if test then do
    content <- folderContents fp
    concat <$> mapM recursiveContents content
  else return []

isJar :: FilePath -> Bool
isJar path =
  takeExtension path == ".jar"
