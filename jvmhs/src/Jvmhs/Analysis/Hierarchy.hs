{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis. Most of the functions in
this module expects a type checking hierarchy.
-}
module Jvmhs.Analysis.Hierarchy
  (
  -- * Hierachy

  -- ** Creation
    Hierarchy -- (..)
  -- , hryNodes
  -- , hryImplements
  -- , hryExtends
  , calculateHierarchy

  -- ** Classes
  , superclasses
  , implementations

  -- ** Fields
  , classNameOfFieldId
  , fieldFromId

  -- ** Methods
  , classNameOfMethodId
  , methodFromId
  , canonicalMethodId
  , callSites
  , methodImpls'
  , methodImpls
  , isMethodRequired

  ) where


import           Control.Lens
import           Control.Lens.Action

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans
import           Control.Monad
import           Data.Monoid
-- import           Data.Set
import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.Query.DFS

import           Jvmhs.ClassPool
import           Jvmhs.Data.Type
import           Jvmhs.Data.Class

data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq)

-- | The class hierarchy analysis results in two graphs, a implements graph and
-- an extends graph.
data Hierarchy = Hierarchy
  { _hryNodes      :: NodeMap ClassName
  , _hryGraph      :: Gr ClassName HEdge
  } deriving (Show, Eq)

-- makeLenses ''Hierarchy

-- | Given a foldable structure over 'ClassName's compute a Hierarchy.
calculateHierarchy ::
  (Foldable t, MonadClassPool m)
  => t ClassName
  -> m Hierarchy
calculateHierarchy clss = do
  (classNames, hierachy) <- clss ^! folded . pool ._Just . to collection
  let
    (nodes_, nodeMap) = mkNodes new classNames
    Just edges_ = mkEdges nodeMap hierachy
  return $ Hierarchy nodeMap (mkGraph nodes_ edges_)
  where
    collection cls =
      let toedge x = to (cls^.className,,x) in
      ( toListOf (className <> classSuper <> classInterfaces.folded) cls
      , toListOf (classSuper.toedge Extend <> classInterfaces.folded.toedge Implement) cls
      )

nodeOf :: Hierarchy -> ClassName -> Node
nodeOf hry = fst . mkNode_ (_hryNodes hry)

-- | Given a Hierarchy, determine the parrents of a class.
-- The returned list is in order as they appear in the class hierarchy.
parrents ::
     Hierarchy
  -> ClassName
  -> [ClassName]
parrents hry@(Hierarchy _ graph) cln =
  let search = dfs [nodeOf hry cln] graph
  in tail $ search ^.. folded . to (lab graph) . _Just

-- | Given a Hierarchy, determine the superclasses of a class.
-- The returned list is in order as they appear in the class hierarchy.
superclasses ::
     Hierarchy
  -> ClassName
  -> [ClassName]
superclasses hry@(Hierarchy _ graph) cln =
  let search = dfs [nodeOf hry cln] (elfilter (==Extend) graph)
  in tail $ search ^.. folded . to (lab graph) . _Just

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  Hierarchy
  -> ClassName
  -> [ClassName]
implementations hry@(Hierarchy _ graph) cln =
  rdfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- | Get the class name of the containing class of a field id.
-- This is delegates to 'fieldFromId'.
classNameOfFieldId ::
  MonadClassPool m
  => AbsFieldId
  -> m (Maybe ClassName)
classNameOfFieldId fid =
  fmap (view (_1.className)) <$> fieldFromId fid

-- | Get the class in which the field resides. The function searches from an
-- initial class.
fieldFromId ::
  MonadClassPool m
  => AbsFieldId
  -> m (Maybe (Class, Field))
fieldFromId afid = go (afid ^. inClassName)
  where
    fid = afid ^. inId
    go "java.lang.Object" =
      return Nothing
    go cn = do
      mc <- getClass cn
      case mc of
        Nothing -> return Nothing
        Just cls -> do
          case cls ^. classField fid of
            Nothing -> go (cls^.classSuper)
            a -> return . fmap (cls,) $ a


-- | Get the class name of the containing class of a method id.
-- This is delegates to 'methodFromId'.
classNameOfMethodId ::
  MonadClassPool m
  => AbsMethodId
  -> m (Maybe ClassName)
classNameOfMethodId mid =
  fmap (view (_1.className)) <$> methodFromId mid

-- | Get the class name and the method of the method id. Starting
-- the search from a class, and proceeds through.
methodFromId ::
  MonadClassPool m
  => AbsMethodId
  -> m (Maybe (Class, Method))
methodFromId amid = do
  go $ amid ^. inClassName
  where
    mid = amid ^. inId
    go cn = do
      mc <- getClass cn
      case mc of
        Nothing -> return Nothing
        Just cls ->
          case cls ^. classMethod mid of
            Nothing
              | cn /= "java.lang.Object" ->
                go $ cls^.classSuper
              | otherwise ->
                return Nothing
            a -> return . fmap (cls,) $ a

canonicalMethodId ::
  MonadClassPool m
  => AbsMethodId
  -> m (Maybe AbsMethodId)
canonicalMethodId amid = do
  x <- methodFromId amid
  return $ asMethodId <$> x

-- | Returns a list of possible call sites.
callSites ::
  MonadClassPool m
  => Hierarchy
  -> AbsMethodId
  -> m [(Class, Method)]
callSites hry mid =
  mid ^!! act canonicalMethodId . _Just . act (methodImpls hry) . folded

-- | Returns all list of pairs of classes and methods that has
-- the same id as the method id.
-- Note: To check if the met
methodImpls' ::
  MonadClassPool m
  => Hierarchy
  -> AbsMethodId
  -> m [(Class, Method)]
methodImpls' hry mn = do
  implementations hry (mn ^. inClassName) ^!! traverse.pool._Just.to mpair._Just
  where mpair cls = (cls ^? classMethod (mn ^. inId) . _Just . to (cls,))

-- | Like 'methodImpls'' with the extra check that all the methods has code
-- executable.
methodImpls ::
  MonadClassPool m
  => Hierarchy
  -> AbsMethodId
  -> m [(Class, Method)]
methodImpls hry mn =
  toListOf (folded.filtered(has $ _2.methodCode._Just)) <$> methodImpls' hry mn


isImplementation ::
  MonadClassPool m
  => Hierarchy
  -> AbsMethodId
  -> m Bool
isImplementation hry mid = do
  mm <- mid ^!? inClassName.pool._Just. classMethod (mid ^. inId) . _Just
  case mm of
    Just method
      | has (methodCode._Just) method -> do
        let clss = parrents hry (mid ^. inClassName)
        Any res <- clss ^! folded . pool
          . to (maybe (Any True)
               (view $ classMethod (mid ^. inId) . like (Any True)))
        return res
    _ ->
      return False

isMethodRequired ::
  MonadClassPool m
  => Hierarchy
  -> AbsMethodId
  -> m Bool
isMethodRequired hry mid =
  fmap (maybe False (const True)) . runMaybeT $
    msum
      [ guard (mid ^. inId . methodIdName == "<clinit>")
      , guard =<< lift (isImplementation hry mid)
      ]
