let
  inherit (import ./common.nix) pkgs gitignoreSource;
in
  pkgs.haskell.packages.ghc883.callCabal2nix "reddup" (gitignoreSource ./.) {}
