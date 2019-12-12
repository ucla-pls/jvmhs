{-# LANGUAGE RankNTypes #-}
module Jvmhs.LensHelpers where

import           Data.Set                      as S
import           Control.Lens

which :: (Getter a Bool) -> Traversal' a a
which l f s = if view l s then f s else pure s

is :: (Eq a) => a -> Getter a Bool
is a = to (== a)

toSet :: Getter a (S.Set a)
toSet = to S.singleton
