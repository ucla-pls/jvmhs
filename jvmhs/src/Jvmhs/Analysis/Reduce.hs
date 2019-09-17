{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis.
-}
module Jvmhs.Analysis.Reduce
  where

-- base
import           Data.Functor
import qualified Data.Set

-- lens
import           Control.Lens

-- hashable
import           Data.Hashable

-- unordered-collections
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- jvmhs
import           Jvmhs
import           Jvmhs.Data.Named

type IFMapping = M.HashMap ClassName (S.HashSet ClassName)

findUnusedInterfaces ::
    (MonadClassPool m)
  => m IFMapping
  -- ^ A map from interfaces to the "correct" replacement of those interfaces.
findUnusedInterfaces = do
  unusedInterfaces <- S.difference <$> findInterfaces <*> findUsedClasses
  unusedICls <- unusedInterfaces ^!! folded . pool . _Just
  return $ M.fromList
    (map (\i -> (i^.name, i^.classInterfaces)) unusedICls)

inlineKey ::
     (Hashable a, Eq a, Foldable t)
  => M.HashMap a (S.HashSet a)
  -> t a
  -> S.HashSet a
inlineKey m =
  foldMap (\i -> M.lookupDefault (S.singleton i) i m)


inlineInterfaces ::
     IFMapping
  -- ^ Collection of interfaces to inline
  -> Class
  -> Class
  -- ^ If class has interface, replace it with the replacement of that interface
inlineInterfaces replaceMap cls =
  let oldInterface = cls^.classInterfaces
  in cls & classInterfaces .~ inlineKey replaceMap oldInterface

toCannoicalIFMapping ::
     (Hashable a, Eq a)
  => M.HashMap a (S.HashSet a)
  -> M.HashMap a (S.HashSet a)
toCannoicalIFMapping m =
  if S.unions (M.elems m) `S.intersection` S.fromMap ( m $> ()) == S.empty
  then m
  else toCannoicalIFMapping $
         M.map (inlineKey m) m

reduceInterfaces ::
    (MonadClassPool m)
  => m ()
reduceInterfaces = do
   interfaces <- toCannoicalIFMapping <$> findUnusedInterfaces
   modifyClasses (Just . inlineInterfaces interfaces)

findInterfaces ::
  (MonadClassPool m)
  => m (S.HashSet ClassName)
findInterfaces =
  foldMap toSetOfInterface <$> allClasses
  where
    toSetOfInterface cls =
      if CInterface `Data.Set.member` (cls ^. classAccessFlags)
      then S.singleton (cls^.name)
      else S.empty

findUsedClasses ::
  (MonadClassPool m)
  => m (S.HashSet ClassName)
findUsedClasses  =
  foldMap findUsedClassesInClass <$> allClasses
  where
    findUsedClassesInClass cls =
      S.fromList $
        toListOf (traverseClass nothing nothing nothing nothing
          (mapAsFieldList.traverse.classNames)
          (mapAsMethodList.traverse.classNames)
          (traverse.classNames)
          nothing
          (traverse.tuple id (traverse.classNames))
          (traverse.classNames) nothing) cls
