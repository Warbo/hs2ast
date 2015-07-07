{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, ArbitraryHaskell, atto-lisp
      , attoparsec, base, bytestring, containers, directory, ghc
      , MissingH, process, QuickCheck, stdenv, stringable, syb, tasty
      , tasty-quickcheck, temporary, transformers, uniplate
      }:
      mkDerivation {
        pname = "HS2AST";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [
          aeson atto-lisp attoparsec base bytestring containers directory ghc
          MissingH process stringable syb temporary transformers uniplate
        ];
        testDepends = [
          aeson ArbitraryHaskell atto-lisp attoparsec base containers
          directory process QuickCheck tasty tasty-quickcheck
        ];
        homepage = "http://chriswarbo.net/essays/repos/hs2ast.html";
        description = "Dump syntax trees of Haskell code";
        license = "GPL";
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
