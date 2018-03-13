{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Jvmhs.Algorithm.DependencyGraph
  ( dependencyGraph
  , testIt
  ) where

import           Data.Graph.Inductive.Dot
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Control.Lens
import           Control.Monad.Trans               (lift)
import           Data.Foldable                     (fold)
import qualified Data.Text                         as Text
-- import Control.Lens.Action
import           Control.Monad.State               (StateT, evalStateT, get,
                                                    modify)
import qualified Data.Map                          as Map
import           Data.Monoid                       ((<>))
import qualified Data.Set                          as Set

import           Jvmhs.ClassReader
import           Jvmhs.Data.Class
import           Jvmhs.Hierarchy
import           Jvmhs.Inspection

data Dependency
  = Implements
  | Extends
  | InField Text.Text
  | InMethod Text.Text
  deriving (Show, Eq)

dependencyGraph
  :: forall r
   . ClassReader r
  => ClassName
  -> Hierarchy r (Gr ClassName Dependency)
dependencyGraph classname = do
  (ns, es) <- depthFirst depends classname
  let m = Map.fromList $ zip ns [1..]
  return $ mkGraph
    (map (\n -> (getFrom m n, n)) ns)
    (map (\(a, b, c) -> (getFrom m a, getFrom m b, c)) es)
  where
    getFrom :: Map.Map ClassName Node -> ClassName -> Node
    getFrom m = maybe 0 id . flip Map.lookup m

    depends cn = do
      cls <- loadClass cn
      let
        super =
          cls ^. classSuper
        interfaces =
          cls ^. classInterfaces
        fieldD =
          cls ^.. classFields
                . traverse
                . fold'in classNames
        methodD =
          cls ^.. classMethods
                . traverse
                . fold'in classNames
      return
        ( super:(interfaces ++ map snd fieldD ++ map snd methodD)
        , ( [cn]
          , (cn, super, Extends) :
            (  map (cn,, Implements) interfaces
            ++ map (\(f, cn') -> (cn, cn', f^.fieldName.to InField)) fieldD
            ++ map (\(m, cn') -> (cn, cn', m^.methodName.to InMethod)) methodD
            )
          )
        )

fold'in :: Fold a b -> Fold a (a, b)
fold'in f = folding $ \a -> map (a,) $ a ^.. f

depthFirst
  :: forall a b m
   . (Ord a, Monoid b, Monad m)
  => (a -> m ([a], b))
  -> a -> m b
depthFirst f =
  flip evalStateT Set.empty . go
  where
    go :: a -> StateT (Set.Set a) m b
    go a = do
      loaded <- get
      if a `Set.member` loaded
        then return mempty
        else do
        modify $ Set.insert a
        (as, b) <- lift (f a)
        (b <>) . fold <$> mapM go as


testIt :: IO ()
testIt = do
  x <-
    runHierarchyInClassPath
      [ "../../jvm-binary/test/data/project"]
      (dependencyGraph (strCls "Main"))

  case x of
    Right (gr, _) -> do
      -- prettyPrint gr

      let dot = fglToDotString (bimap (Text.unpack . classNameAsText) show gr)
      writeFile "test.dot" (showDot dot)

    Left err -> do
      print err
