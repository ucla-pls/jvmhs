module Language.JVM.Binary.Helpers where

import qualified Data.Vector     as V

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

getVector :: Binary e => Get (V.Vector e)
getVector = do
  len <- getInt16be
  V.replicateM (fromIntegral len) get

putVector :: Binary e => V.Vector e -> Put
putVector vector = do
  putInt16be (fromIntegral $ V.length vector)
  V.forM_ vector put
