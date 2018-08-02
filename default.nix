{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
, local-basic-host ? false
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  reflex-basic-host-sources = if local-basic-host then ../reflex-basic-host else (import ./nix/reflex-basic-host.nix);

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage reflex-basic-host-sources {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-brick.nix {};
in
  drv
