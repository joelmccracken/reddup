let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell rec {
    nativeBuildInputs = with pkgs; [
      cabal2nix
      haskell.compiler.ghc844
      haskellPackages.ghcid
    ];
  }
