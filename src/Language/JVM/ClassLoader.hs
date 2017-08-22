{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
-- This module contains the virtual class loader

module Language.JVM.ClassLoader
  ( ClassLoader (..)
  , loadClass
  , fromClassPath
  , fromJreFolder

  , preloadClasses
  , loadClassFromPreloader
  , ClassPreloader (..)
  ) where

import           Language.JVM.Class
import           Language.JVM.ClassName
import           Language.JVM.Method
import           System.Directory
import           System.FilePath
import           System.Process

import           Control.Lens

import           Control.Monad          (forM, forM_)

import qualified Data.Vector            as V

import           Data.Maybe             (catMaybes)

import           Debug.Trace

import qualified Data.ByteString.Lazy   as BL

import           Codec.Archive.Zip

import qualified Data.Map.Lazy          as M

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
      test <- doesFileExist (pathOfClass path cn)
      if test
        then do
          bc <- BL.readFile (pathOfClass path cn)
          return $ Just (path, bc)
        else return Nothing


-- Class preloader

data PreloadedClass
  = ArchiveClass FilePath Archive
  | PathClass FilePath
  deriving (Show)

newtype ClassPreloader = ClassPreloader
  { classMap :: M.Map ClassName [PreloadedClass]
  } deriving (Show)

preloadClasses :: ClassLoader -> IO ClassPreloader
preloadClasses cl = do
  classes <- forM (paths cl) preloadpath
  return . ClassPreloader . M.fromListWith (++) . concat $ classes
  where
    preloadpath fp
      | isJar fp = do
          arc <- readZipFile fp
          let xs = catMaybes . map fromFile $ filesInArchive arc
          return $ map (, [ArchiveClass fp arc]) xs
      | True = do
          xs <- catMaybes . map fromFile <$> folderContents fp
          return $ map (, [PathClass fp]) xs

    fromFile :: FilePath -> Maybe ClassName
    fromFile fp = do
      case splitExtension fp of
        (filepath, ".class") -> return $ fromStr filepath
        _                    -> Nothing

loadClassFromPreloader
  :: ClassName
  -> ClassPreloader
  -> IO [(FilePath, Either String Class)]
loadClassFromPreloader cn (ClassPreloader cpl) =
  case M.lookup cn cpl of
    Just plc -> concat <$> mapM (loadPreloadedClass cn) plc
    Nothing  -> return []

loadPreloadedClass
  :: ClassName
  -> PreloadedClass
  -> IO [(FilePath, Either String Class)]
loadPreloadedClass cn plc =
  reader <$> case plc of
    ArchiveClass fp arch ->
      return $ (fp,) . fromEntry <$>
         maybe [] (:[]) (findEntryByPath (pathOfClass "" cn) arch)

    PathClass fp -> do
      let path = (pathOfClass fp cn)
      test <- doesFileExist path
      if test
        then do
          bs <- BL.readFile path
          return [(fp, bs)]
        else return []

   where
     reader = fmap (\(fp, bs) -> (fp, decodeClassOrFail bs))


-- Helpers
jarsFromFolder :: FilePath -> IO [ FilePath ]
jarsFromFolder fp =
  filter isJar <$> folderContents fp

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
