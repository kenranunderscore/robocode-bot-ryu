{
  description = "Development environment for Ryu, a Robocode Tank Royale bot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import inputs.nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            pkgs.haskell.packages.ghc94.haskell-language-server
            pkgs.cabal-install
            pkgs.haskell.compiler.ghc94
            pkgs.zlib
            pkgs.nixfmt
          ];
        };
      });
}
