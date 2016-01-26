{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "traversable-scan";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/ttuegel/traversable-scan";
  description = "Prefix sums (scans) for any Traversable structure";
  license = stdenv.lib.licenses.bsd3;
}
