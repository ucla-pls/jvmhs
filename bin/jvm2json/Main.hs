{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Conedec

import Jvmhs.Format.Codec

main :: IO ()
main =
  debugCodec @V1 (ref @"ByteCodeInst")
