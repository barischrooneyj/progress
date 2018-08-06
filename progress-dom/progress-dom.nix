{ mkDerivation, base, reflex-dom, stdenv }:
mkDerivation {
  pname = "progress-dom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base reflex-dom ];
  executableHaskellDepends = [ base reflex-dom ];
  testHaskellDepends = [ base reflex-dom ];
  homepage = "https://github.com/barischrooneyj/progress-dom#readme";
  license = stdenv.lib.licenses.bsd3;
}
