{ mkDerivation, ArbitraryHaskell, base, bytestring, Cabal
, containers, directory, ghc, ghc-paths, MissingH, process
, QuickCheck, stdenv, syb, tasty, tasty-quickcheck, temporary
, transformers, uniplate
}:
mkDerivation {
  pname = "HS2AST";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ArbitraryHaskell base bytestring Cabal containers directory ghc
    ghc-paths process QuickCheck syb temporary transformers uniplate
  ];
  testDepends = [
    ArbitraryHaskell base bytestring Cabal containers directory ghc
    ghc-paths MissingH process QuickCheck syb tasty tasty-quickcheck
    temporary transformers uniplate
  ];
  homepage = "http://chriswarbo.net/essays/repos/hs2ast.html";
  description = "Dump syntax trees of Haskell code";
  license = "GPL";
}
