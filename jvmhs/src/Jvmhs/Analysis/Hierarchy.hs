{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis.
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

  -- ** Accesses
  , superclasses
  , implementations

  -- ** Helpers
  , classNameOfFieldId
  , fieldFromId
  , classNameOfMethodId
  , methodFromId
  ) where


import           Control.Lens

import           Data.Monoid
-- import           Data.Set
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
  (classNames, hierachy) <- clss ^! folded . load . to collection
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

-- -- | Given a Hierarchy, determine the subclasses of a class.
-- subclasses ::
--      Hierarchy
--   -> ClassName
--   -> [ClassName]
-- subclasses hry@(Hierarchy _ graph) cln =
--   tail $ rdfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- | Given a Hierarchy, determine the superclasses of a class.
-- The returned list is in order as they appear in the class hierarchy.
superclasses ::
     Hierarchy
  -> ClassName
  -> [ClassName]
superclasses hry@(Hierarchy _ graph) cln =
  tail $ dfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- | Returns a list of all classes that implement some interface, or extends
-- a class.
implementations ::
  Hierarchy
  -> ClassName
  -> [ClassName]
implementations hry@(Hierarchy _ graph) cln =
  tail $ rdfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- -- | Returns a list of all interfaces that a class implements.
-- superinterfaces ::
--   Hierarchy
--   -> ClassName
--   -> [ClassName]
-- superinterfaces hry@(Hierarchy _ graph) cln =
--   tail $ dfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- | Get the class name of the containing class of a field id.
-- This is delegates to 'fieldFromId'.
classNameOfFieldId ::
  MonadClassPool m
  => FieldId
  -> ClassName
  -> m (Maybe ClassName)
classNameOfFieldId fid cn =
  fmap (view (_1.className)) <$> fieldFromId fid cn

-- | Get the class in which the field resides. The function
-- searches from the class
fieldFromId ::
  MonadClassPool m
  => FieldId
  -> ClassName
  -> m (Maybe (Class, Field))
fieldFromId fid cn =
  flip catchError (const $ return Nothing) $ do
    cls <- loadClass cn
    case cls ^? classField fid of
      Just f ->
        return $ Just (cls, f)
      Nothing
        | cn /= "java.lang.Object"
          -> fieldFromId fid (cls^.classSuper)
        | otherwise
          -> return $ Nothing

-- | Get the class name of the containing class of a method id.
-- This is delegates to 'methodFromId'.
classNameOfMethodId ::
  MonadClassPool m
  => MethodId
  -> ClassName
  -> m (Maybe ClassName)
classNameOfMethodId fid cn =
  fmap (view (_1.className)) <$> methodFromId fid cn

-- | Get the class name and the method of the method id. Starting
-- the search from a class, and proceeds through.
methodFromId ::
  MonadClassPool m
  => MethodId
  -> ClassName
  -> m (Maybe (Class, Method))
methodFromId fid cn =
  flip catchError (const $ return Nothing) $ do
    cls <- loadClass cn
    case cls ^? classMethod fid of
      Just f ->
        return $ Just (cls, f)
      Nothing
        | cn /= "java.lang.Object"
          -> methodFromId fid (cls^.classSuper)
        | otherwise
          -> return Nothing

-- -- | Get the class in which the method resides.
-- classOfField ::
--   MonadClassPool m
--   => ClassName
--   -> FieldId
--   -> m ClassName
-- classOfField cn fid = do
--   cls <- loadClass cn
--   if has (classField fid) cls
--     then return $ cn
--     else classOfField (cls^.superClass) fid
