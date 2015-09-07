{ mkDerivation, aeson, atto-lisp, attoparsec, base, bytestring
, derive, ghc, QuickCheck, stdenv, stringable, syb, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "HS2AST";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson atto-lisp attoparsec base bytestring ghc stringable syb
  ];
  testHaskellDepends = [
    aeson atto-lisp attoparsec base derive ghc QuickCheck stringable
    tasty tasty-quickcheck
  ];
  homepage = "http://chriswarbo.net/essays/repos/hs2ast.html";
  description = "Dump syntax trees of Haskell code";
  license = "GPL";
}
