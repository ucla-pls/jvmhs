-- This module contains the virtual class loader

module Language.JVM.ClassLoader where

import           Language.JVM.Class
import           Language.JVM.ClassName
import           System.Directory
import           System.FilePath
import           System.Process

import           Debug.Trace

import qualified Data.ByteString.Lazy   as BL

import           Codec.Archive.Zip

import qualified Data.Map.Lazy          as Map

data ClassLoader = ClassLoader
  { classpath :: [ FilePath ]
  , ext       :: FilePath
  , lib       :: FilePath
  } deriving (Show, Eq)

simple :: [ FilePath ] -> IO ClassLoader
simple fps = do
  java <- readProcess "which" ["java"] ""
  return $ fromJreFolder (takeDirectory (takeDirectory java) </> "jre") fps

fromJreFolder :: FilePath -> [ FilePath ] -> ClassLoader
fromJreFolder jre clspath = do
  ClassLoader clspath (jre </> "lib/ext") (jre </> "lib")

findClasses :: ClassLoader -> IO [ ClassName ]
findClasses (ClassLoader cp ext rt) =
  concat <$> sequence (map findClassesInFolder cp)

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

readZipFile :: FilePath -> IO Archive
readZipFile fp = do
  toArchive <$> BL.readFile fp

readZipFileContent :: FilePath -> IO [ FilePath ]
readZipFileContent fp = do
  filesInArchive <$> readZipFile fp


loadClass :: ClassName -> ClassLoader -> IO Class
loadClass cn cl = do
  bootjars <- filter isJar <$> folderContents (lib cl)
  extjars <- filter isJar <$> folderContents (ext cl)
  let paths = bootjars ++ extjars ++ classpath cl

  traceIO $ show paths
  go paths
  where
    go (path:rest) | isJar path = do
       traceIO $ show path
       arch <- readZipFile path
       case findEntryByPath (pathOfClass "" cn) arch of
         Just entry' ->
           case decodeClassOrFail . fromEntry $ entry' of
             Right cls -> return cls
             Left msg  -> go rest
         _ -> go rest
    go (path:rest) = do
      let pc = pathOfClass path cn
      test <- doesFileExist pc
      if test then do
        res <- decodeClassOrFailFrom $ pc
        case res of
          Right cls -> return cls
          Left msg  -> go rest
       else go rest
    go [] = fail $ "Could not load " ++ show cn

    isJar path = takeExtension path == ".jar"


xClass :: String -> IO Class
xClass str =
  loadClass (fromDotStr str) =<< simple [ "test-suite/project" ]

folderContents :: FilePath -> IO [ FilePath ]
folderContents fp = map (fp </>) <$> listDirectory fp
