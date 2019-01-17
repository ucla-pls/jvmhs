{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, deepseq, directory, fgl, fgl-visualize, filepath, generic-random
, hashable, hpack, hspec, hspec-discover
, hspec-expectations-pretty-diff, jvm-binary, lens, lens-action
, mtl, process, QuickCheck, stdenv, tasty, text, transformers
, unordered-containers, vector, zip-archive
}:
mkDerivation {
  pname = "jvmhs";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers deepseq directory fgl
    fgl-visualize filepath hashable jvm-binary lens lens-action mtl
    process text transformers unordered-containers vector zip-archive
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers deepseq directory fgl
    fgl-visualize filepath generic-random hashable hspec hspec-discover
    hspec-expectations-pretty-diff jvm-binary lens lens-action mtl
    process QuickCheck tasty text transformers unordered-containers
    vector zip-archive
  ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  homepage = "https://github.com/ucla-pls/jvmhs#readme";
  description = "A library for reading Java class-files";
  license = stdenv.lib.licenses.bsd3;
}