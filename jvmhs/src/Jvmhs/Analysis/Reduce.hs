{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis.
-}
module Jvmhs.Analysis.Reduce
  where

import            Control.Monad.IO.Class
import            Control.Lens
import            Jvmhs
import            Jvmhs.Data.Code
import            Data.Foldable
import            Data.Traversable
import            Data.Int
import            Data.Word
import            Data.Text.Internal
import            Data.List

import qualified Data.Set                             as S

import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.Code          as B
import qualified Language.JVM.Attribute.StackMapTable as B
import qualified Language.JVM.Constant                as B
import qualified Language.JVM.Stage                   as B


findMethod :: Class -> MethodId -> Maybe Method
findMethod cls mid =
  preview (classMethods.folded.filtered(view $ toMethodId.to(== mid))) cls


javaLangObject::B.ClassName
javaLangObject = B.ClassName {B.classNameAsText = "java/lang/Object"}

reduceInterface :: [FilePath] -> FilePath -> ClassName -> IO ()
reduceInterface classPath outputPath mainClass = do
  let cl = ClassLoader [] [] classPath
  pl <- preload cl
  Right (x,hs) <- flip runClassPool' (emptyState pl) $ do
    (found, missing) <- computeClassClosure (S.singleton  mainClass)
    clsLst <- traverse loadClass (S.toList found)
    let itfLst = findInterfaces clsLst
    let usedClsLst = findUsedClasses clsLst
    let iftsToRemove = S.difference itfLst usedClsLst
    let foundClsNames =
          map
          (^.className)
          (filter (\c ->  B.CInterface `notElem` (c^.classAccessFlags) ) clsLst)

    _ <- traverse (\itf ->
      do
        removedInterface <- loadClass itf
        let superItfcToAdd = toList (S.difference (S.fromList $ removedInterface ^. classInterfaces) iftsToRemove)
        traverse (\clsName ->
          do
            cls <- loadClass clsName
            let newInterfaces = superItfcToAdd ++ filter (/= itf) (cls ^. classInterfaces)
            when ( itf `elem` (cls ^. classInterfaces)) $
              modifyClass (cls ^. className) $ classInterfaces .~ newInterfaces
          ) foundClsNames

      ) (S.toList iftsToRemove)

--    newCls1 <- loadClass "Simple"
--    liftIO $ print newCls1

    loadClass mainClass
--    (found2, missing2) <- computeClassClosure (S.singleton "Simple")
--    liftIO $ print found2
  savePartialClassPoolState outputPath (S.singleton mainClass) hs
--  saveHierarchyState "test/output" hs

findInterfaces ::
  Foldable t
  => t Class
  -> S.Set ClassName
findInterfaces = foldMap toSetOfInterface
  where
    toSetOfInterface cls =
      if CInterface `S.member` (cls ^. classAccessFlags)
      then S.singleton (cls^.className)
      else S.empty

findUsedClasses ::
  Foldable t
  => t Class
  -> S.Set ClassName
findUsedClasses  =
  foldMap findUsedClassesInClass
  where
    findUsedClassesInClass cls =
      S.fromList $
        toListOf (traverseClass nothing nothing nothing nothing
          (traverse.classNames)
          (traverse.classNames)
          (traverse.classNames)
          nothing) cls