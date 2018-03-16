# default.nix
{ pkgs ? import <nixpkgs> {} }:

(pkgs.haskell.packages.ghc822.callCabal2nix "inline-c-template" ./. {}).env