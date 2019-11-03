{ mkDerivation, aeson, ansi-wl-pprint, base, base16-bytestring
, binary, bytestring, cassava, containers, cryptohash-sha256
, deepseq, fgl, filepath, hexstring, hpack, jvmhs, lens
, lens-action, mtl, optparse-applicative, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "javaq";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring binary bytestring
    cassava containers cryptohash-sha256 deepseq fgl filepath hexstring
    jvmhs lens lens-action mtl optparse-applicative text
    unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson ansi-wl-pprint base base16-bytestring binary bytestring
    cassava containers cryptohash-sha256 deepseq fgl filepath hexstring
    jvmhs lens lens-action mtl optparse-applicative text
    unordered-containers vector
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/ucla-pls/jvmhs#readme";
  description = "A library for reading Java class-files";
  license = stdenv.lib.licenses.bsd3;
}
