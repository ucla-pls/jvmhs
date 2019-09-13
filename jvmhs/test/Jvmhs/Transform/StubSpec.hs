-- |
{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Transform.StubSpec where

import SpecHelper

import Jvmhs
import Jvmhs.Transform.Stub

spec :: Spec
spec = do
  describe "stub"
    $ useOutputFolder "test/output/stub"
    $ forEveryClassIt "can stub all methods" $ \cls -> do
      let cls' = cls & classMethodList . traverse %~ stub
      writeClass "test/output/stub" cls'
