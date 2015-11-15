{ mkDerivation, base, base-unicode-symbols, containers
, containers-unicode-symbols, fingertree, HUnit, integer-gmp
, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text

, criterion
}:
mkDerivation {
  pname = "numerals";
  version = "HEAD";
  src = ./.;

  libraryHaskellDepends = [
    base base-unicode-symbols containers containers-unicode-symbols
    fingertree integer-gmp text
  ];

  testHaskellDepends = [
    base base-unicode-symbols HUnit integer-gmp QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2 text

    criterion
  ];
  homepage = "https://github.com/roelvandijk/numerals";
  description = "Convert numbers to number words";
  license = stdenv.lib.licenses.bsd3;
}
