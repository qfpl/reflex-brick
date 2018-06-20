{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-basic-host = self.callPackage (import ./nix/reflex-basic-host.nix) {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-brick.nix {};
in
  drv
