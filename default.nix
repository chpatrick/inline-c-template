{ pkgs ? import <nixpkgs> {} }:

let drv = (pkgs.haskell.packages.ghc822.callCabal2nix "inline-c-template" ./. {});
in if pkgs.lib.inNixShell then drv.env else drv
