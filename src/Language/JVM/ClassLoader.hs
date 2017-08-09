-- This module contains the virtual class loader

module Language.JVM.ClassLoader where

import Language.JVM.Class

data ClassLoader = ClassLoader
  { path :: [ FilePath ]
  } deriving (Show, Eq)

single :: FilePath -> ClassLoader
single fp = ClassLoader [fp]

loadClass :: ClassName -> ClassLoader -> IO Class
loadClass cn cl = do
  go (path cl)
  where
    go (path:rest) = do
      res <- decodeClassOrFail $ pathOfClass path cn
      case res of
        Right cls -> return cls
        Left msg -> go rest
    go [] = fail $ "Could not load " ++ show cn
