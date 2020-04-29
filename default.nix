{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
, jvm-binary ? import ./nix/jvm-binary.nix
}: 
let 
  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = builtins.filterSource 
      (path: type: baseNameOf path != ".nix")
      ./.;
    name = "jvmhs";
    source-overrides = { inherit jvm-binary; };
    overrides = hsuper: hself: {
    };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }
