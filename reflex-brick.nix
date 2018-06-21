{ mkDerivation, base, brick, lens, mtl, reflex, reflex-basic-host
, stdenv, stm, vty
}:
mkDerivation {
  pname = "reflex-brick";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick lens mtl reflex reflex-basic-host stm vty
  ];
  executableHaskellDepends = [ base brick lens mtl reflex vty ];
  license = stdenv.lib.licenses.bsd3;
}
