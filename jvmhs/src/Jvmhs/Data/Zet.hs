{-|
Module      : Jvmhs.Data.Zet
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A Zet is a special zet of sets, where it is easy to find the smallest
possible union of sets st a property has been uphold.
-}

module Jvmhs.Data.Zet where

import           Control.Lens
import           Control.Monad
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Functor
import qualified Data.IntSet   as IS
import qualified Data.List     as L
import qualified Data.Vector   as V

type Z = IS.IntSet

class Zet zz where
  fromListOfSet :: [Z] -> zz
  splitZ :: MonadPlus m => (Z -> m ()) -> zz -> m (zz, Z, zz)
  conditionZ :: zz -> Z -> zz
  filterZ :: Int -> zz -> zz
  mergeZ :: zz -> zz -> zz
  totalZ :: zz -> Z

newtype ZZ = ZZ (V.Vector Z)
instance Zet ZZ where
  fromListOfSet zs =
    ZZ (V.fromList . L.sortOn IS.size $ zs)

  splitZ p (ZZ z) = do
    i <- binarySearch' p (V.postscanl IS.union IS.empty z)
    return $ (ZZ ( V.take i z), (z V.! i), ZZ(V.drop (i + 1) z))

  conditionZ (ZZ mu) z = fromListOfSet $ mu ^.. traverse . to (IS.union z)
  filterZ k (ZZ zz) = ZZ (V.filter ((<k).IS.size) zz)
  mergeZ (ZZ zz1) (ZZ zz2) = (ZZ $ zz1 V.++ zz2)
  totalZ (ZZ zz) = V.foldr IS.union IS.empty zz

newtype MZ = MZ (Z, V.Vector Z)
instance Zet MZ where
  fromListOfSet zs =
    MZ (IS.empty, V.fromList . L.sortOn IS.size $ zs)

  splitZ p (MZ(b, z)) = do
    i <- binarySearch' (p . IS.union b) (V.postscanl IS.union IS.empty z)
    return $ (MZ (b, V.take i z), (z V.! i) `IS.union` b, MZ(b, V.drop (i + 1) z))

  conditionZ (MZ(a, mu)) z =
    let MZ (_, zz) = fromListOfSet $ mu ^.. traverse . to (IS.\\ z)
    in MZ (a `IS.union` z, zz)

  filterZ k (MZ (b, zz))=
    MZ (b, V.filter ((<k - bs).IS.size) zz)
    where bs = IS.size b

  mergeZ (MZ (a, zz1)) (MZ (_, zz2)) = (MZ (a, zz1 V.++ zz2))

  totalZ (MZ (_, zz)) = V.foldr IS.union IS.empty zz


-- | A union Zet, is a binary tree of sets, where the branches holds
-- the nodes of the sets.
data UnionZet
  = UZBranch Z UnionZet UnionZet
  | UZNode Z
  | UZEmpty

newtype CUZ = CUZ (Z, UnionZet)

toListZet :: UnionZet -> [Z]
toListZet z =
  go z []
  where
    go (UZEmpty) = id
    go (UZNode x) = (x:)
    go (UZBranch _ lz rz) =
      go lz . go rz


-- mergeUZ :: UnionZet -> UnionZet -> UnionZet
-- mergeUZ UZEmpty a = a
-- mergeUZ a UZEmpty = a
-- mergeUZ lz rz =
--   UZBranch (totalUZ lz `IS.union` totalUZ rz) lz rz

mergeUZ :: UnionZet -> UnionZet -> UnionZet
mergeUZ x y = fromListOfSetUZ (toListZet x ++  toListZet y)

totalUZ :: UnionZet -> Z
totalUZ (UZBranch z _ _) = z
totalUZ (UZNode z) = z
totalUZ (UZEmpty) = IS.empty

fromListOfSetUZ :: [IS.IntSet] -> UnionZet
fromListOfSetUZ zs =
  go (L.length zs) $ L.sortOn IS.size zs
  where
    go _ [x] = UZNode x
    go n xs =
      let
        p = (n `quot` 2)
        (l, r) = L.splitAt p xs
        lz = go p l
        rz = go (n - p) r
      in UZBranch (totalUZ lz `IS.union` totalUZ rz) lz rz

instance Zet CUZ where
  fromListOfSet zs =
    CUZ (IS.empty, fromListOfSetUZ zs)

  splitZ p (CUZ (c, z)) = do
    (lz, m, rz) <- go c z
    return (CUZ (c, lz), m `IS.union` c, CUZ (c, rz))
    where
      go _ (UZEmpty) = mzero
      go _ (UZNode x) = return $ (UZEmpty, x, UZEmpty)
      go b (UZBranch _ lz rz) =
        ( do
            p (b `IS.union` totalUZ lz)
            (lhs, r, rhs) <- go b lz
            return (lhs, r, rhs `mergeUZ` rz)
        )
        <|>
        ( do
            (lhs, r, rhs) <- go (b `IS.union` totalUZ lz) rz
            return (lz `mergeUZ` lhs, r, rhs)
        )

  conditionZ (CUZ (c', z)) c = CUZ (c `IS.union` c', go z)
    where
      go UZEmpty = UZEmpty
      go (UZNode x) = UZNode (x IS.\\ c)
      go x@(UZBranch tz lz rz)
        | IS.null (tz `IS.intersection` c) = x
        | IS.null (totalUZ rz `IS.intersection` c) =
          go lz `mergeUZ` rz
        | otherwise =
          fromListOfSetUZ [ s IS.\\ c | s <- toListZet x]

  filterZ k (CUZ (c, z)) = CUZ (c, go z)
    where
      csize = IS.size c
      go (UZEmpty) = UZEmpty
      go (UZNode x) =
        if IS.size x < k - csize then UZNode x else UZEmpty
      go (UZBranch _ lz rz) =
        case go rz of
          UZEmpty -> go lz
          a -> lz `mergeUZ` a


  mergeZ (CUZ (c, z)) (CUZ (_, z2)) = CUZ (c, z `mergeUZ` z2)

  totalZ (CUZ (c, z)) = c `IS.union` totalUZ z


binarySearch ::
  (Monad m)
  => (e -> m Bool)
  -> V.Vector e
  -> m (Maybe Int)
binarySearch p =
  runMaybeT . binarySearch' (lift . p >=> guard)

-- | Given a vector of elements more and more true for a predicate, give the smallest
-- index such that the predicate is satisfied.
binarySearch' ::
  (MonadPlus m)
  => (e -> m ())
  -> V.Vector e
  -> m Int
binarySearch' p vec =
  go 0 (V.length vec)
  where
    go i j
      | i == j =
        guard (i < V.length vec) $> i
      | otherwise =
        let pivot = i + ((j - i) `quot` 2)
        in msum
         [ p (vec V.! pivot) >> go i pivot
         , go (pivot + 1) j
         ]
