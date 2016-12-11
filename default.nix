{ mkDerivation, base, deepseq, hspec, newtype, QuickCheck, stdenv
, vector, vector-th-unbox
}:
mkDerivation {
  pname = "sigym4-null";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base deepseq newtype vector ];
  testHaskellDepends = [
    base hspec newtype QuickCheck vector vector-th-unbox
  ];
  homepage = "https://github.com/meteogrid/sigym4-null";
  description = "Values that represent the absence of a value with a sentinel value (aka \"nodata\")";
  license = stdenv.lib.licenses.bsd3;
}
