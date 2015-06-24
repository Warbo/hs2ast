module HS2AST.Tests.Mod2AST where

import Data.Char
import Data.List
import Data.List.Utils
import Data.String.Utils
import Mod2AST
import Test.Arbitrary.Cabal
import Test.Arbitrary.Haskell
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.Tasty
import Test.Tasty.QuickCheck

projectHasPkgs pkgs = all (`elem` deps) pkgs'
  where pkgs'       = filter (\x -> all isAlpha x && x /= "") pkgs
        project     = genProject pkgs' "testMain"
        S _ exec    = head (sections project)
        Just depStr = lookup "build-depends" exec
        deps        = split "," depStr

projectHasMain main = ("main = " ++ main') `elem` lines mainHs
  where main'           = filter (/= '\n') main
        Just (H mainHs) = lookup ([], "Main.hs") (files (genProject [] main'))

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
  , testProperty "Can set main function" projectHasMain
  ]

impureTests = testGroup "Impure Mod2AST tests" [
    testProperty "hs2astDeps matches our Cabal file" hs2astDependenciesMatch
  ]
