{ mkDerivation, base, linear, stdenv, tasty, tasty-quickcheck
, vector
}:
mkDerivation {
  pname = "traversable-scan";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base linear tasty tasty-quickcheck vector ];
  homepage = "https://github.com/ttuegel/traversable-scan";
  description = "Prefix sums (scans) for any Traversable structure";
  license = stdenv.lib.licenses.bsd3;
}
