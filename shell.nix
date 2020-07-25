let
  inherit (import ./common.nix) pkgs ghcide;
in
  pkgs.mkShell rec {
    nativeBuildInputs = with pkgs; [
      cabal2nix
      haskell.compiler.ghc883
      haskellPackages.ghcid
      ghcide
    ];
  }
