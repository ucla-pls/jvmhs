module Language.JVM.Binary.Constant
  ( Constant (..)
  , ConstantRef
  , getConstantRef
  , putConstantRef
  , ConstantPool (..)
  ) where

import Control.Monad (forM_)

import qualified Data.ByteString    as BS
import           Data.Int
import qualified Data.IntMap.Strict as IM
import           Data.Word

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

type ConstantRef = Int16

data Constant
  = String BS.ByteString
  | Integer !Word32
  | Float !Word32
  | Long !Word64
  | Double !Word64
  | ClassRef !ConstantRef
  | StringRef !ConstantRef
  | FieldRef ConstantRef ConstantRef
  | MethodRef ConstantRef ConstantRef
  | InterfaceMethodRef ConstantRef ConstantRef
  | NameAndType ConstantRef ConstantRef
  | MethodHandle Word8 ConstantRef
  | MethodType ConstantRef
  | InvokeDynamic ConstantRef ConstantRef
  deriving (Show, Eq)


instance Binary Constant where
  get = do
    ident <- getWord8
    case ident of
      1 -> do
        len <- getInt16be
        String <$> getByteString (fromIntegral len)
      3 -> Integer <$> getWord32be
      4 -> Float <$> getWord32be
      5 -> Long <$> getWord64be
      6 -> Double <$> getWord64be
      7 -> ClassRef <$> getConstantRef
      8 -> StringRef <$> getConstantRef
      9 -> FieldRef <$> getConstantRef <*> getConstantRef
      10 -> MethodRef <$> getConstantRef <*> getConstantRef
      11 -> InterfaceMethodRef <$> getConstantRef <*> getConstantRef
      12 -> NameAndType <$> getConstantRef <*> getConstantRef
      15 -> MethodHandle <$> getWord8 <*> getConstantRef
      16 -> MethodType <$> getConstantRef
      18 -> InvokeDynamic <$> getConstantRef <*> getConstantRef
      _ -> error $ "Unkown identifier " ++ show ident

  put x =
    case x of
      String bs -> do
        putWord8 1
        putInt16be . fromIntegral $ BS.length bs
        putByteString bs
      Integer i -> do
        putWord8 3
        putWord32be i
      Float i -> do
        putWord8 4
        putWord32be i
      Long i -> do
        putWord8 5
        putWord64be i
      Double i -> do
        putWord8 6
        putWord64be i
      ClassRef i -> do
        putWord8 7
        putConstantRef i
      StringRef i -> do
        putWord8 8
        putConstantRef i
      FieldRef i j -> do
        putWord8 9
        putConstantRef i
        putConstantRef j
      MethodRef i j -> do
        putWord8 10
        putConstantRef i
        putConstantRef j
      InterfaceMethodRef i j -> do
        putWord8 11
        putConstantRef i
        putConstantRef j
      NameAndType i j -> do
        putWord8 12
        putConstantRef i
        putConstantRef j
      MethodHandle i j -> do
        putWord8 15
        putWord8 i
        putConstantRef j
      MethodType i -> do
        putWord8 16
        putConstantRef i
      InvokeDynamic i j -> do
        putWord8 18
        putConstantRef i
        putConstantRef j

poolSize :: Constant -> Int
poolSize (Double _) = 2
poolSize (Long _) = 2
poolSize _ = 1

getConstantRef :: Get ConstantRef
getConstantRef = getInt16be

putConstantRef :: ConstantRef -> Put
putConstantRef = putInt16be

newtype ConstantPool = ConstantPool
  { unConstantPool :: IM.IntMap Constant
  } deriving (Show, Eq)

instance Binary ConstantPool where
  get = do
    len <- fromIntegral <$> getInt16be
    -- traceM $ "read length: " ++ show len
    list <- go len 1
    return . ConstantPool $ IM.fromList list
    where
      go len n | len > n = do
        constant <- get
        -- traceM $ "read constant: " ++ show constant
        rest <- go len (n + poolSize constant)
        return $ (n, constant) : rest
      go _ _ = return []
  put (ConstantPool p)= do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        -- traceM $ "wrote length: " ++ show (key + poolSize e)
        putInt16be (fromIntegral (key + poolSize e))
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0
