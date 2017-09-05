{-# LANGUAGE DeriveFunctor #-}
module Language.JVM.Binary.SizedList
  ( SizedList16 (..)
  , SizedList32 (..)
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Control.Monad

newtype SizedList16 a = SizedList16
  { unSizedList16 :: [ a ]
  } deriving (Show, Eq, Functor)

instance Foldable SizedList16 where
  foldMap am =
    foldMap am . unSizedList16

instance Traversable SizedList16 where
  traverse afb ta =
    SizedList16 <$> traverse afb (unSizedList16 ta)

instance Binary a => Binary (SizedList16 a) where
  get = do
    len <- getInt16be
    SizedList16 <$> replicateM (fromIntegral len) get

  put (SizedList16 sl) = do
    putInt16be (fromIntegral $ length sl)
    forM_ sl put

newtype SizedList32 a = SizedList32
  { unSizedList32 :: [ a ]
  } deriving (Show, Eq, Functor)

instance Foldable SizedList32 where
  foldMap am =
    foldMap am . unSizedList32

instance Traversable SizedList32 where
  traverse afb ta =
    SizedList32 <$> traverse afb (unSizedList32 ta)

instance Binary a => Binary (SizedList32 a) where
  get = do
    len <- getInt32be
    SizedList32 <$> replicateM (fromIntegral len) get

  put (SizedList32 sl) = do
    putInt32be (fromIntegral $ length sl)
    forM_ sl put
