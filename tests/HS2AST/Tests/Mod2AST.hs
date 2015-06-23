module HS2AST.Tests.Mod2AST where

import Data.List
import Data.List.Utils
import Data.String.Utils
import Mod2AST
import Test.Arbitrary.Cabal
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.Tasty
import Test.Tasty.QuickCheck

projectHasPkgs pkgs = all (`elem` deps) pkgs
  where project     = genProject pkgs
        S _ exec    = head (sections project)
        Just depStr = lookup "build-depends" exec
        deps        = split "," depStr

hs2astDependenciesMatch = monadicIO $ do
  cbl <- run $ readFile "HS2AST.cabal"
  let deps = extractCabalField "build-depends" cbl
  assert (all (`elem` deps) hs2astDeps)

extractCabalField fld cbl = map strip . split "," . unlines $ fst:rst
  where fld' = fld ++ ":"
        -- Remove everything before the first occurence of this field
        pre  = dropWhile (not . (fld' `isInfixOf`)) (lines cbl)
        -- Remove everything up to and including the ':'
        fst  = tail . dropWhile (/= ':') . head $ pre
        -- Keep any subsequent lines, until we find one containing ':'
        rst  = takeWhile (not . (':' `elem`)) . tail $ pre

pureTests   = testGroup "Pure Mod2AST tests" [
    testProperty "All packages become build-depends" projectHasPkgs
  ]

impureTests = testGroup "Impure Mod2AST tests" [
    testProperty "hs2astDeps matches our Cabal file" hs2astDependenciesMatch
  ]
