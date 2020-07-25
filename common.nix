let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {
    config = {};
  };
  gis = import sources.gitignore { inherit (pkgs) lib; };

  ghcide = pkgs.haskell.packages.ghc883.ghcide;

  self = {
    inherit (pkgs) niv;
    inherit pkgs ghcide;
    inherit (gis) gitignoreSource;
  };

in
  self
