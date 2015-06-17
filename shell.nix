{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ArbitraryHaskell, base, bytestring
      , containers, directory, ghc, ghc-paths, QuickCheck, stdenv, syb
      , tasty, tasty-quickcheck, temporary, transformers, uniplate
      }:
      mkDerivation {
        pname = "HS2AST";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          ArbitraryHaskell base bytestring containers directory ghc ghc-paths
          QuickCheck syb transformers uniplate
        ];
        testDepends = [
          ArbitraryHaskell base bytestring containers directory ghc ghc-paths
          QuickCheck syb tasty tasty-quickcheck temporary transformers
          uniplate
        ];
        homepage = "http://chriswarbo.net/essays/repos/hs2ast.html";
        description = "Dump syntax trees of Haskell code";
        license = "GPL";
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
