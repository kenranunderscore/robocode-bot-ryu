{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: {
    devShells.x86_64-linux.default =
      let pkgs = nixpkgs.legacyPackages.x86_64-linux; in
      pkgs.mkShell {
        nativeBuildInputs = [ pkgs.cabal-install pkgs.haskell.compiler.ghc94 pkgs.zlib ];
      };
  };
}
