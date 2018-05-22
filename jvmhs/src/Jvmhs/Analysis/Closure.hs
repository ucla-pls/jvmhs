{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Jvmhs.Analysis.Closure
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

The purpose of this module is to compute closures over some
items. Closures should be seen as contrast to reductions, that
where reductions remove items, closures adds items until
-}

module Jvmhs.Analysis.Closure
  ( computeClassClosure
  ) where

import Data.Either (partitionEithers)

import Control.Lens

import qualified Data.Set as S

import Jvmhs.Inspection
import Jvmhs.Data.Class
import Jvmhs.Data.Type
import Jvmhs.ClassPool


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
        (notexists, exists) <- partitionEithers <$> wave ^!! folded . load'
        let
          found = S.fromList $ exists^..folded.className
          missed = S.fromList $ notexists^..folded.heClassName
          known' = known `S.union` found
          unknown' = unknown `S.union` missed
          front = S.fromList $ exists^..folded.classNames
        go (known', unknown') (front S.\\ known')
