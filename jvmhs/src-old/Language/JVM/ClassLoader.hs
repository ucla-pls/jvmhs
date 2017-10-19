{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-|
Module      : Jvmhs.Data.ClassLoader
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

-}

module Jvmhs.Data.ClassLoader
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
import           System.Directory
import           System.FilePath
import           System.Process

import           Control.Lens
import           Data.Monoid

import           Data.Maybe             (catMaybes)

import qualified Data.ByteString.Lazy   as BL

import           Codec.Archive.Zip

import qualified Data.Map.Lazy          as M

data ClassLoader = ClassLoader
  { _lib       :: [ FilePath ]
  , _ext       :: [ FilePath ]
  , _classpath :: [ FilePath ]
  } deriving (Show, Eq)

makeLenses ''ClassLoader

-- Constructors

-- | Creates a 'ClassLoader' from a class path, automatically predicts
-- the java version used using the 'which' command.
fromClassPath :: [ FilePath ] -> IO ClassLoader
fromClassPath fps = do
  java <- readProcess "which" ["java"] ""
  fromJreFolder fps $ takeDirectory (takeDirectory java) </> "jre"

-- | Creates a 'ClassLoader' from a classpath
fromJreFolder :: [ FilePath ] -> FilePath -> IO ClassLoader
fromJreFolder clspath jre =
  ClassLoader
    <$> (jarsFromFolder $ jre </> "lib")
    <*> (jarsFromFolder $ jre </> "lib/ext")
    <*> pure clspath

type ClassReader = ClassName -> IO [(FilePath, Either String Class)]

-- | Returns the paths in the order they should be checked for classes
paths
  :: (Functor f, Monoid (f ClassLoader))
  => ([FilePath] -> f [FilePath])
  -> ClassLoader
  -> f ClassLoader
paths = lib <> ext <> classpath

 -- <> ext <> classpath

loadClass :: ClassLoader -> ClassReader
loadClass cl cn = do
  x <- mapM tryload $ cl ^. paths
  return $ catMaybes x
  where
    tryload path = do
      bc <- readPath path
      return $ case bc of
        Just (path', bs) ->
          Just (path', decodeClassOrFail bs)
        Nothing ->
          Nothing

    readPath path | isJar path = do
       arch <- readZipFile path
       return $ (path,) . fromEntry <$>
         findEntryByPath (pathOfClass "" cn) arch

    readPath path = do
      test <- doesFileExist (pathOfClass path cn)
      if test
        then do
          bc <- BL.readFile (pathOfClass path cn)
          return $ Just (path, bc)
        else return Nothing


-- ClassPreloader

data PreloadedClass
  = ArchiveClass FilePath Archive
  | PathClass FilePath
  deriving (Show)

newtype ClassPreloader = ClassPreloader
  { classMap :: M.Map ClassName [PreloadedClass]
  } deriving (Show)

preloadClasses :: ClassLoader -> IO ClassPreloader
preloadClasses cl = do
  classes <- mapM preloadpath $ cl ^. paths
  return . ClassPreloader . M.fromListWith (++) . concat $ classes
  where
    preloadpath fp
      | isJar fp = do
          arc <- readZipFile fp
          let xs = catMaybes . map fromFile $ filesInArchive arc
          return $ map (, [ArchiveClass fp arc]) xs
      | True = do
          xs <- catMaybes . map fromFile <$> recursiveContents fp
          return $ map (, [PathClass fp]) xs

    fromFile :: FilePath -> Maybe ClassName
    fromFile fp = do
      case splitExtension fp of
        (filepath, ".class") -> return $ fromStr filepath
        _                    -> Nothing


loadClassFromPreloader
  :: ClassPreloader
  -> ClassReader
loadClassFromPreloader (ClassPreloader cpl) cn = do
  concat <$> mapM (loadPreloadedClass cn) (cpl ^. at cn . _Just)

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

-- readZipFileContent :: FilePath -> IO [ FilePath ]
-- readZipFileContent fp = do
--   filesInArchive <$> readZipFile fp

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
