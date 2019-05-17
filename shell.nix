{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
, local-basic-host ? true
} :
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  drv = import ./. { inherit reflex-platform compiler local-basic-host; };
in
  if pkgs.lib.inNixShell then drv.env else drv
