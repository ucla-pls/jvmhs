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
import            Control.Monad
import            Jvmhs

import qualified Data.Set                             as S
import qualified Data.Map                             as M
import qualified Data.Vector                          as V

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

chopSet :: Ord x => Int -> S.Set x -> S.Set (S.Set x)
chopSet n s
  | divisible = chopSet' blockSize n s S.empty
  | otherwise = chopSet' blockSize (vectorLength `mod` n) s S.empty
  where blockSize = vectorLength `roundUpQuot` n
        vectorLength = length s
        divisible =  length s `mod` n == 0

chopSet' ::
     Ord x
  => Int
  -- ^ size of each Set (delta)
  -> Int
  -- ^ num of Set remaining should be larger
  -> S.Set x
  -> S.Set (S.Set x)
  -> S.Set (S.Set x)

chopSet' i n s rslt
  | S.null s       = rslt
  | otherwise         = let (block, rest) = S.splitAt newBlockSize s
                          in chopSet'
                            newBlockSize newN rest (S.insert block rslt )
  where newN         = if n > -1 then n-1 else n
        newBlockSize = if n == 0 then i-1 else i



roundUpQuot :: Int -> Int -> Int
roundUpQuot i j = ceiling ((fromIntegral i / fromIntegral j)::Float)

ddmin ::
     (Monad m, Ord x)
--     (Monad m, MonadIO m, Show x)
  => S.Set x
  -> (S.Set x -> m Bool)
  -- ^ a function can test delta
  -> m(S.Set x)

ddmin world = ddmin' world 2

ddmin' ::
     (Monad m, Ord x)
--     (Monad m, MonadIO m, Show x)
  => S.Set x
  -> Int
  -> (S.Set x -> m Bool)
  -- ^ a function can test delta
  -> m(S.Set x)
ddmin' world n testFunc
  | S.size world == 1 = return world
  | otherwise = do
      firstDelta <- getFirst testFunc deltaSet
      firstDeltaCompl <- getFirst testFunc deltaComplSet
      case (firstDelta, firstDeltaCompl) of
        (Just a, _)        ->
          ddmin' a 2 testFunc
        (Nothing, Just a)  ->
          ddmin' a (max (n-1) 2) testFunc
        (Nothing, Nothing)
          | n < S.size world ->
              ddmin' world (min (S.size world) (2*n)) testFunc
          | otherwise ->
              return world
      where deltaSet      = chopSet n world
            deltaComplSet = S.map (S.difference world) deltaSet
            sHead         = S.elemAt 0


getFirst ::
    (Foldable t, Monad m)
 => (S.Set x -> m Bool)
 -> t (S.Set x)
 -> m (Maybe (S.Set x))

getFirst testFun =
  foldM fun Nothing
  where
    fun b a = case (b, a) of
                (Nothing, _) -> do
                    rsl <- testFun a
                    if rsl
                    then
                      return $ Just a
                    else
                      return Nothing
                (Just a', _) -> return (Just a')
