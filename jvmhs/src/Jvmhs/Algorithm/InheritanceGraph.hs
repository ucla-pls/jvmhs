{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Jvmhs.Algorithm.InheritanceGraph
  ( inheritanceGraph
  , testIt
  ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Dot

import Control.Monad (foldM)
import Control.Monad.Trans (lift)
import Control.Lens
-- import Control.Lens.Action
import Control.Monad.State (StateT, evalStateT, modify, get)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Monoid ((<>))
import qualified Data.Text as Text

import Jvmhs.Hierarchy
import Jvmhs.ClassReader
import Jvmhs.Data.Class

data IEdge
  = Implements
  | Extends
  deriving (Show, Eq)

inheritanceGraph
  :: forall r
   . ClassReader r
  => ClassName
  -> Hierarchy r (Gr ClassName IEdge)
inheritanceGraph classname = do
  (ns, es) <- evalStateT (go classname) Set.empty
  let m = Map.fromList $ zip ns [1..]
  return $ mkGraph
    (map (\n -> (getFrom m n, n)) ns)
    (map (\(a, b, c) -> (getFrom m a, getFrom m b, c)) es)
  where
    getFrom :: Map.Map ClassName Node -> ClassName -> Node
    getFrom m = maybe 0 id . flip Map.lookup m

    go :: ClassName
      -> StateT
         (Set.Set ClassName)
         (Hierarchy r)
         ([ClassName], [(ClassName, ClassName, IEdge)])
    go cn = do
      loaded <- get
      if cn `Set.member` loaded
        then
        return ([], [])
        else do
          modify $ Set.insert cn
          cls <- lift $ loadClass cn
          let inf = cls ^. classSuper
          res  <- go inf
          res' <- foldM (\r i -> (r <>) <$> go i) mempty $ cls ^. classInterfaces
          return $
            ([cn], (cn, inf, Extends): map (cn,,Implements) (cls ^. classInterfaces))
            <> res <> res'

-- visitOnce
--   :: forall r a
--    . (ClassReader r, Monoid a)
--   => (ClassName -> Hierachy r a)
--   -> ClassName -> Hierarchy r a
-- visitOnce f cn=
--   evalStateT (go cn) Set.empty
--   where
--     go :: ClassName -> StateT (Set.Set ClassName) (Hierarchy r) a
--     go cn = do
--       loaded <- get
--       if cn `Set.member` loaded
--         then return mempty
--         else do

testIt :: IO ()
testIt = do
  Right (gr, _) <-
    runHierarchyInClassPath
      [ "../jvm-binary/test-suite/data/project"]
      (inheritanceGraph (strCls "Main"))

  let dot = fglToDotString (bimap (Text.unpack . classNameAsText) show gr)
  writeFile "test.dot" (showDot dot)
  prettyPrint gr
