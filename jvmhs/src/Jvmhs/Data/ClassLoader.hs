{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Jvmhs.Data.ClassLoader
  ( ClassReader (..)
  , readClass

  , ClassLoader (..)
  , fromClassPath
  , fromJreFolder
  , paths

  -- , preloadClasses
  -- , loadClassFromPreloader
  -- , ClassPreloader (..)
  ) where

import           System.Directory
import           System.FilePath
import           System.Process

import           Control.Lens
import           Data.Monoid

import           Data.Maybe             (catMaybes)
-- import           Data.Bifunctor

import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text as Text

import           Codec.Archive.Zip

-- import qualified Data.Map.Lazy          as M

import           Jvmhs.Data.Class
import qualified Language.JVM as B

data ClassReadError
 = ClassNotFound
 | MalformedClass String
 deriving (Show, Eq)

class ClassReader m where
  readClassFile :: m -> ClassName -> IO (Either ClassReadError B.ClassFile)

readClass :: (ClassReader m) => m -> ClassName -> IO (Either ClassReadError Class)
readClass m cn = do
  ocls <- readClassFile m cn
  return $ do
    cls <- ocls
    let _ = cls :: B.ClassFile
    (cls ^. checked) & _Left %~ MalformedClass

data ClassLoader = ClassLoader
  { _lib       :: [ FilePath ]
  , _ext       :: [ FilePath ]
  , _classpath :: [ FilePath ]
  } deriving (Show, Eq)

makeLenses ''ClassLoader

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

-- | Returns the paths in the order they should be checked for classes
paths
  :: (Functor f, Monoid (f ClassLoader))
  => ([FilePath] -> f [FilePath])
  -> ClassLoader
  -> f ClassLoader
paths = lib <> ext <> classpath

data ClassContainer
  = CCFolder FilePath
  | CCJar FilePath Archive
  deriving (Show)

container :: FilePath -> IO (Maybe ClassContainer)
container path
  | isJar path = do
      arch <- readZipFile path
      return $ CCJar path <$> (arch ^? _Right)
  | otherwise = do
      test <- doesDirectoryExist path
      return $ if test
        then Just (CCFolder path)
        else Nothing

containers :: ClassLoader -> IO [ ClassContainer ]
containers cl = do
  c <- sequence $ cl ^.. paths . traverse . to container
  return $ catMaybes c

instance ClassReader (ClassContainer) where
  readClassFile (CCFolder fp) cn = do
    let cls = pathOfClass fp cn
    x <- doesFileExist cls
    if x
      then do
        file <- BL.readFile cls
        return $ B.decodeClassFile file & _Left %~ MalformedClass
      else return $ Left ClassNotFound
  readClassFile (CCJar _ arch) cn = do
    case findEntryByPath (pathOfClass "" cn) arch of
      Just f ->
        return $ B.decodeClassFile (fromEntry f) & _Left %~ MalformedClass
      Nothing ->
        return $ Left ClassNotFound

instance ClassReader (ClassLoader) where
  readClassFile cl cn =
    go =<< containers cl
    where
      go (p:ps) = do
        rcf <- readClassFile p cn
        case rcf of
          Right cls ->
            return $ Right cls
          Left (MalformedClass _) ->
            return rcf
          Left ClassNotFound ->
            go ps
      go [] =
        return $ Left ClassNotFound

instance ClassReader FilePath where
  readClassFile fp cn = do
    x <- container fp
    case x of
      Just s ->
        readClassFile s cn
      Nothing ->
        return $ Left ClassNotFound

-- -- Constructors


-- type ClassReader = ClassName -> IO [(FilePath, Either String Class)]


-- loadClass :: ClassLoader -> ClassReader
-- loadClass cl cn = do
--   x <- mapM tryload $ cl ^. paths
--   return $ catMaybes x
--   where
--     tryload path = do
--       bc <- readPath path
--       return $ case bc of
--         Just (path', bs) ->
--           Just (path', decodeClassOrFail bs)
--         Nothing ->
--           Nothing

--     readPath path | isJar path = do
--        arch <- readZipFile path
--        return $ (path,) . fromEntry <$>
--          findEntryByPath (pathOfClass "" cn) arch

--     readPath path = do
--       test <- doesFileExist (pathOfClass path cn)
--       if test
--         then do
--           bc <- BL.readFile (pathOfClass path cn)
--           return $ Just (path, bc)
--         else return Nothing


-- -- ClassPreloader

-- data PreloadedClass
--   = ArchiveClass FilePath Archive
--   | PathClass FilePath
--   deriving (Show)

-- newtype ClassPreloader = ClassPreloader
--   { classMap :: M.Map ClassName [PreloadedClass]
--   } deriving (Show)

-- preloadClasses :: ClassLoader -> IO ClassPreloader
-- preloadClasses cl = do
--   classes <- mapM preloadpath $ cl ^. paths
--   return . ClassPreloader . M.fromListWith (++) . concat $ classes
--   where
--     preloadpath fp
--       | isJar fp = do
--           arc <- readZipFile fp
--           let xs = catMaybes . map fromFile $ filesInArchive arc
--           return $ map (, [ArchiveClass fp arc]) xs
--       | True = do
--           xs <- catMaybes . map fromFile <$> recursiveContents fp
--           return $ map (, [PathClass fp]) xs

--     fromFile :: FilePath -> Maybe ClassName
--     fromFile fp = do
--       case splitExtension fp of
--         (filepath, ".class") -> return $ classNameFromStr filepath
--         _                    -> Nothing


-- loadClassFromPreloader
--   :: ClassPreloader
--   -> ClassReader
-- loadClassFromPreloader (ClassPreloader cpl) cn = do
--   concat <$> mapM (loadPreloadedClass cn) (cpl ^. at cn . _Just)

-- loadPreloadedClass
--   :: ClassName
--   -> PreloadedClass
--   -> IO [(FilePath, Either String Class)]
-- loadPreloadedClass cn plc =
--   reader <$> case plc of
--     ArchiveClass fp arch ->
--       return $ (fp,) . fromEntry <$>
--          maybe [] (:[]) (findEntryByPath (pathOfClass "" cn) arch)

--     PathClass fp -> do
--       let path = (pathOfClass fp cn)
--       test <- doesFileExist path
--       if test
--         then do
--           bs <- BL.readFile path
--           return [(fp, bs)]
--         else return []

--    where
--      reader = fmap (\(fp, bs) -> (fp, decodeClassOrFail bs))

-- | Path
pathOfClass :: FilePath -> ClassName -> FilePath
pathOfClass fp cn =
  fp ++ "/" ++ Text.unpack (classNameAsText cn) ++ ".class"

classNameFromPath :: String -> ClassName
classNameFromPath = ClassName . Text.pack

-- -- Helpers
jarsFromFolder :: FilePath -> IO [ FilePath ]
jarsFromFolder fp =
  filter isJar <$> folderContents fp

-- | Read a zip file
readZipFile :: FilePath -> IO (Either String Archive)
readZipFile fp = do
  toArchiveOrFail <$> BL.readFile fp

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
