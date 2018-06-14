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


chopVector :: Int -> V.Vector x -> V.Vector (V.Vector x)
chopVector n vec
  | divisible = chopVector' blockSize n vec V.empty
  | otherwise = chopVector' blockSize (vectorLength `mod` n) vec V.empty
  where blockSize = vectorLength `roundUpQuot` n
        vectorLength = length vec
        divisible =  length vec `mod` n == 0

chopVector' ::
     Int
  -- ^ size of each Vec (delta)
  -> Int
  -- ^ num of vec remaining should be larger
  -> V.Vector x
  -> V.Vector (V.Vector x)
  -> V.Vector (V.Vector x)

chopVector' i n vec rslt
  | V.null vec        = rslt
  | otherwise         = let (block, rest) = V.splitAt newBlockSize vec
                          in chopVector'
                            newBlockSize newN rest (snoc rslt block)
  where newN         = if n > -1 then n - 1 else n
        newBlockSize = if n == 0 then i-1 else i

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j = ceiling ((fromIntegral i / fromIntegral j)::Float)

deltaI ::
     V.Vector (V.Vector x)
  -> Int
  -> V.Vector x
deltaI vecs i =
  vecs V.! i

deltaComplementI ::
    V.Vector (V.Vector x)
  -> Int
  -> V.Vector x
deltaComplementI vecs i =
  V.foldl1 (V.++) $ V.ifilter (\j _ -> i /= j) vecs


ddmin ::
     Monad m
--     (Monad m, MonadIO m, Show x)
  => V.Vector x
  -> (V.Vector x -> m Bool)
  -- ^ a function can test delta
  -> m(V.Vector x)

ddmin world = ddmin' world 2

ddmin' ::
     Monad m
--     (Monad m, MonadIO m, Show x)
  => V.Vector x
  -> Int
  -> (V.Vector x -> m Bool)
  -- ^ a function can test delta
  -> m(V.Vector x)
ddmin' world n testFunc
  | length world == 1 = return world
  | otherwise = do
      deltaTrueVec <- V.filterM testFunc deltaVec
      deltaComplTrueVec <- V.filterM testFunc deltaComplVec
      if V.length world == 1
      then return world
      else if not $ null deltaTrueVec
           then
             ddmin' (V.head deltaTrueVec) 2 testFunc
           else if not $ null deltaComplTrueVec
             then
               ddmin' (V.head deltaComplTrueVec) (max (n-1) 2) testFunc
             else if n < V.length world
               then
                  ddmin' world (min (V.length world) (2*n)) testFunc
               else
                  return world

      where enumVec       = V.fromList [0..(n-1)]
            vecs          = chopVector n world
            deltaVec      = V.map (deltaI vecs) enumVec
            deltaComplVec = V.map (deltaComplementI vecs) enumVec



--is178 :: (Monad m, Num x, Eq x) => V.Vector x -> m Bool
--is178 v =
-- return $ and [(V.elem 1 v), (V.elem 7 v), (V.elem 8 v)]
--
--
----deltaTest :: (Monad m, MonadIO m) => m ()
--deltaTest :: IO()
--deltaTest = do
--  let numVec = V.fromList [1..8]
--  rslt <- ddmin numVec is178
--  print rslt
--  return ()