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

--import            Control.Monad.IO.Class
import            Control.Lens
import            Jvmhs

import qualified Data.Set                             as S
import qualified Data.Map                             as M

type IFMapping = M.Map ClassName (S.Set ClassName)

findUnusedInterfaces ::
    (MonadClassPool m)
  => m IFMapping
  -- ^ A map from interfaces to the "correct" replacement of those interfaces.
findUnusedInterfaces = do
  unusedInterfaces <- S.difference <$> findInterfaces <*> findUsedClasses
  unusedICls <- unusedInterfaces ^!! folded . pool . _Just
  return $ M.fromList
    (map (\i -> (i^.className, S.fromList $ i^.classInterfaces)) unusedICls)

inlineKey ::
     (Ord a, Foldable t)
  => M.Map a (S.Set a)
  -> t a
  -> S.Set a
inlineKey m =
  foldMap (\i -> M.findWithDefault (S.singleton i) i m)


inlineInterfaces ::
     IFMapping
  -- ^ Collection of interfaces to inline
  -> Class
  -> Class
  -- ^ If class has interface, replace it with the replacement of that interface
inlineInterfaces replaceMap cls =
  let oldInterface = cls^.classInterfaces
  in cls & classInterfaces .~
    S.toList (inlineKey replaceMap oldInterface)

toCannoicalIFMapping ::
     Ord a
  => M.Map a (S.Set a)
  -> M.Map a (S.Set a)
toCannoicalIFMapping m =
  if S.unions (M.elems m) `S.intersection` M.keysSet m == S.empty
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
  => m (S.Set ClassName)
findInterfaces =
  foldMap toSetOfInterface <$> allClasses
  where
    toSetOfInterface cls =
      if CInterface `S.member` (cls ^. classAccessFlags)
      then S.singleton (cls^.className)
      else S.empty

findUsedClasses ::
  (MonadClassPool m)
  => m (S.Set ClassName)
findUsedClasses  =
  foldMap findUsedClassesInClass <$> allClasses
  where
    findUsedClassesInClass cls =
      S.fromList $
        toListOf (traverseClass nothing nothing nothing nothing
          (traverse.classNames)
          (traverse.classNames)
          (traverse.classNames)
          nothing) cls

