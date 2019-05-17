{ mkDerivation, base, brick, bytestring, containers, dependent-map
, dependent-sum, dependent-sum-template, lens, linear, mtl, random
, reflex, reflex-basic-host, stdenv, stm, vty
}:
mkDerivation {
  pname = "reflex-brick";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick bytestring dependent-map dependent-sum
    dependent-sum-template lens mtl reflex reflex-basic-host stm vty
  ];
  executableHaskellDepends = [
    base brick containers lens linear mtl random reflex vty
  ];
  description = "Integrating reflex and brick";
  license = stdenv.lib.licenses.bsd3;
}
