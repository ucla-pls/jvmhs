{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Jvmhs.Analysis.Closure
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The purpose of this module is to compute closures over some items. Closures
should be seen as contrast to reductions, that where reductions remove items,
closures adds items until everything that needs to be added have been added.
-}

module Jvmhs.Analysis.Closure
  ( computeClassClosure
  , computeMethodClosure

  , ClassGraph
  , mkClassGraph

  , CallGraph
  , mkCallGraph

  ) where

import           Data.Either              (partitionEithers)


import           Control.Lens
import           Control.Lens.Action

import qualified Data.Set                 as S

import           Data.Set.Lens            (setOf)

import           Jvmhs.Analysis.Hierarchy
import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type
import           Jvmhs.Inspection

import Debug.Trace

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
  => S.Set ClassName
  -> m (S.Set ClassName, S.Set ClassName)
computeClassClosure =
  go (S.empty, S.empty)
  where
    go (!known, !unknown) !wave
      | S.null wave = do
        return (known, unknown)
      | otherwise = do
        -- List of all the classes that exists
        (notexists, exists) <- partitionEithers <$> wave ^!! folded . pool'
        let
          found = setOf (folded.className) exists
          missed = S.fromList notexists
          known' = known `S.union` found
          unknown' = unknown `S.union` missed
          front = setOf (folded.classNames) exists
        go (known', unknown') (front S.\\ known')

type CallGraph = Graph AbsMethodId ()

-- | Given a foldable structure over 'AbsMethodId's compute a CallGraph.
mkCallGraph ::
  forall m. (MonadClassPool m)
  => Hierarchy
  -> m ([AbsMethodId], CallGraph)
mkCallGraph hry = do
  methods <- concat <$>
    mapClasses (\c -> [(asMethodId (c, m), setOf methodNames m) | m <- c^.classMethodList])
  let (missing, edges) = partitionEithers $
          methods ^.. folded
                    . to (\(mid, m) -> [(mid,,()) <$> getDeclartion mid' | mid' <- S.toList m ])
                    . folded
  return (missing, mkGraph (methods^..folded._1) edges)
  where
    getDeclartion mid =
      case declaration hry mid of
        Just x -> Right x
        Nothing -> Left mid


-- | Computes the method closure
computeMethodClosure ::
  forall m. MonadClassPool m
  => Hierarchy
  -> S.Set AbsMethodId
  -> m (S.Set AbsMethodId)
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
            found = setOf (folded . _1) impls
            known' = known `S.union` found
            front = setOf (folded . _2 . methodNames) impls
          go known' (front S.\\ known')
