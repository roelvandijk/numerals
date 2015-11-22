{ mkDerivation
, base
, containers
, fingertree
, HUnit
, integer-gmp
, QuickCheck
, stdenv
, test-framework
, test-framework-hunit
, test-framework-quickcheck2
, text

, criterion
}:
mkDerivation {
  pname = "numerals";
  version = "HEAD";
  src = ./.;

  libraryHaskellDepends = [
    base
    containers
    fingertree
    integer-gmp
    text
  ];

  testHaskellDepends = [
    base
    HUnit
    integer-gmp
    QuickCheck
    test-framework
    test-framework-hunit
    test-framework-quickcheck2
    text

    criterion
  ];
  homepage = "https://github.com/roelvandijk/numerals";
  description = "Convert numbers to number words";
  license = stdenv.lib.licenses.bsd3;
}
