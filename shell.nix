with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, containers, directory, ghc
             , ghc-paths, haskell-generate, QuickCheck, stdenv, syb, tasty
             , tasty-quickcheck, temporary, transformers, uniplate
             }:
             mkDerivation {
               pname = "HS2AST";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base bytestring containers directory ghc ghc-paths QuickCheck syb
                 transformers uniplate
               ];
               testDepends = [
                 base bytestring containers directory ghc ghc-paths haskell-generate
                 QuickCheck syb tasty tasty-quickcheck temporary transformers
                 uniplate
               ];
               homepage = "http://chriswarbo.net/essays/repos/hs2ast.html";
               description = "Dump syntax trees of Haskell code";
               license = "GPL";
             }) {};
in
  pkg.env
