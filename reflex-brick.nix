{ mkDerivation, base, brick, lens, mtl, reflex, reflex-basic-host
, stdenv, stm, vty
}:
mkDerivation {
  pname = "reflex-brick";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base brick lens mtl reflex reflex-basic-host stm vty
  ];
  license = stdenv.lib.licenses.bsd3;
}
