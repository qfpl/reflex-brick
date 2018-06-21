{ mkDerivation, base, brick, bytestring, dependent-map
, dependent-sum, lens, mtl, reflex, reflex-basic-host, stdenv, vty
}:
mkDerivation {
  pname = "reflex-brick";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick bytestring dependent-map dependent-sum lens mtl reflex
    reflex-basic-host vty
  ];
  executableHaskellDepends = [ base brick lens mtl reflex vty ];
  license = stdenv.lib.licenses.bsd3;
}
