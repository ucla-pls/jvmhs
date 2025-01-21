{
  description = "A library for reading Java class-files";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jvm-binary = {
      url = "github:ucla-pls/jvm-binary";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cones = {
      url = "github:kalhauge/cones";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    packages = p: {
      "jvmhs" = p.callCabal2nixWithOptions "jvmhs" self "" {};
      "jvm-binary" = p.callCabal2nixWithOptions "jvm-binary" inputs.jvm-binary "" {};
    };
    overlays = final: prev: {
      haskellPackages = prev.haskellPackages.extend (p: _: packages p);
      jvm2json = final.haskell.lib.justStaticExecutables final.haskellPackages.jvmhs;
    };
  in
    {
      overlays = {
        default = overlays;
        all = [overlays inputs.cones.overlays.default];
      };
    }
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs =
        import nixpkgs
        {
          inherit system;
          overlays = [overlays inputs.cones.overlays.default];
        };
      hpkgs = pkgs.haskellPackages;
    in {
      packages = {
        default = hpkgs.jvmhs;
        inherit (hpkgs) jvmhs;
      };
      devShells = let
        buildInputs = with hpkgs; [
          cabal-install
          ghcid
          haskell-language-server
          hpack
          fourmolu
        ];
        withHoogle = true;
      in {
        default =
          hpkgs.shellFor
          {
            name = "jvmhs-shell";
            packages = p: [p.jvmhs];
            inherit buildInputs withHoogle;
          };
      };
    });
}
