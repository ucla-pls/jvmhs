{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
}: 
let 
  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = ./.;
    name = "jvmhs";
    source-overrides = {
      reduce = ../reduce;
    };
    overrides = hsuper: hself: {
      dirtree = haskellPackages.callHackageDirect { 
        pkg = "dirtree";
        ver = "0.1.3";
        sha256 = "sha256:0kl31l2ip856saq5lhfjr4wv04i2pyj6sf3lkzk9bj7w1m9v0klz";
      } {};
    };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }
