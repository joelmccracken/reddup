let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  gis = import sources.gitignore { inherit (pkgs) lib; };

  ghcide = pkgs.haskell.packages.ghc883.callPackage ./nix/ghcide.nix {};

  self = {
    inherit (pkgs) niv;
    inherit pkgs ghcide;
    inherit (gis) gitignoreSource;
  };

in
  self
