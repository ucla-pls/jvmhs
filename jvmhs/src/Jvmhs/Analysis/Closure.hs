{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Jvmhs.Analysis.Closure
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

The purpose of this module is to compute closures over some items. Closures
should be seen as contrast to reductions, that where reductions remove items,
closures adds items until everything that needs to be added have been added.
-}

module Jvmhs.Analysis.Closure
  ( computeClassClosure
  , computeClassClosureM
  , computeMethodClosure

  , ClassGraph
  , mkClassGraph

  , CallGraph
  , mkCallGraph

  ) where

import           Data.Either              (partitionEithers)

import           Control.Lens
import           Control.Lens.Action

-- unordered-collections
import qualified Data.HashSet                 as S

import           Data.Set.Lens            (setOf)

import           Jvmhs.Analysis.Hierarchy
import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type
import           Jvmhs.Inspection
import           Jvmhs.Data.Named

type ClassGraph = Graph ClassName ()

-- | Given a foldable structure over 'ClassName's compute a ClassGraph.
mkClassGraph ::
  MonadClassPool m
  => m ClassGraph
mkClassGraph = do
  clss <- allClassNames
  mkGraph clss <$> collectClasses outEdges
  where
    outEdges cls =
      let cn = cls^.className
      in cls^.classNames.to (S.singleton.(cn,,()))

-- | Computes the class closure in the current space.
-- It will only include classes known to the 'MonadClassPool'.
computeClassClosure ::
  MonadClassPool m
  => S.HashSet ClassName
  -> m (S.HashSet ClassName, S.HashSet ClassName)
computeClassClosure =
  flip computeClassClosureM (const $ return ())

-- | Computes the class closure in the current space.
-- It will only include classes known to the 'MonadClassPool'.
computeClassClosureM ::
  MonadClassPool m
  => S.HashSet ClassName
  -> (([ClassName], [Class]) -> m ())
  -> m (S.HashSet ClassName, S.HashSet ClassName)
computeClassClosureM cls fm =
  go (S.empty, S.empty) cls
  where
    go (!known, !unknown) !wave
      | S.null wave = do
        return (known, unknown)
      | otherwise = do
        -- List of all the classes that exists
        res@(notexists, exists) <- partitionEithers <$> wave ^!! folded . pool'
        fm res
        let
          found = S.fromList $ toListOf (folded.className) exists
          missed = S.fromList notexists
          known' = known `S.union` found
          unknown' = unknown `S.union` missed
          front = S.fromList $ toListOf (folded.classNames) exists
        go (known', unknown') (front `S.difference` known')


type CallGraph = Graph AbsMethodName ()

-- | Given a foldable structure over 'AbsMethodName's compute a CallGraph.
mkCallGraph ::
  forall m. (MonadClassPool m)
  => Hierarchy
  -> m ([AbsMethodName], CallGraph)
mkCallGraph hry = do
  methods <- concat <$>
    mapClasses (\c -> [((c ^. name, m ^. name), S.fromList $ toListOf methodNames m) | m <- c^.classMethodList])
  let (missing, edges) = partitionEithers $
          methods ^.. folded
                    . to (\(mid, m) -> [(mid,,()) <$> getDeclartion mid' | mid' <- S.toList m ])
                    . folded
  return (missing, mkGraph (methods^..folded._1) edges)
  where
    getDeclartion mid =
      case declaration hry mid of
        Just x  -> Right x
        Nothing -> Left mid


-- | Computes the method closure
computeMethodClosure ::
  forall m. MonadClassPool m
  => Hierarchy
  -> S.HashSet AbsMethodName
  -> m (S.HashSet AbsMethodName)
computeMethodClosure hry =
  go S.empty
  where
    go !known !wave
      | S.null wave = do
          return known
      | otherwise = do
          -- First get all possible implementation
          let mths = setOf (folded . to (callSites hry) . folded) wave
          impls <- mths ^!! folded.(selfIndex <. act getMethod . folded) . withIndex
          let
            found = S.fromList $ toListOf (folded . _1) impls
            known' = known `S.union` found
            front = S.fromList $ toListOf (folded . _2 . methodNames) impls
          go known' (front `S.difference` known')
