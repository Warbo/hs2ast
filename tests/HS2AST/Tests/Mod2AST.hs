module HS2AST.Tests.Mod2AST where

import Control.Exception (SomeException, try)
import Data.Char
import Data.List
import Data.List.Utils
import Data.String.Utils
import Mod2AST
import System.Directory
import System.Exit
import System.Process
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
  let deps  = extractCabalField "build-depends" cbl
      match = all (`elem` map stripVersion deps)
                         (map stripVersion hs2astDeps)
  run $ if match then return ()
                 else print (("hs2astDeps", hs2astDeps),
                             ("extracted", deps))
  assert match

extractCabalField fld cbl = split "," . unlines $ fst:rst
  where fld' = fld ++ ":"
        -- Remove everything before the first occurence of this field
        pre  = dropWhile (not . (fld' `isInfixOf`)) (lines cbl)
        -- Remove everything up to and including the ':'
        fst  = tail . dropWhile (/= ':') . head $ pre
        -- Keep any subsequent lines, until we find one containing ':'
        rst  = takeWhile (not . (':' `elem`)) . tail $ pre

stripVersion x = takeWhile (/= ' ') (strip x)

haveCabal = do
  found <- try (readProcessWithExitCode
                  "cabal"
                  ["--help"]
                  "") :: IO (Either SomeException (ExitCode, String, String))
  case found of
       Left  _ -> print "Cannot run 'cabal', skipping test" >> return False
       Right _ -> return True

actionsAreRun :: String -> Int -> Property
actionsAreRun main expected = monadicIO $ do
  result <- run $ mkCabalIn "hs2asttest" [] main (const (return expected))
  assert (result == expected)

projectDirMade main = monadicIO $ do
  exists <- run $ mkCabalIn "hs2asttest" [] main doesDirectoryExist
  assert exists

projectDirCleanedUp main = monadicIO $ do
  dir <- run $ mkCabalIn "hs2asttest" [] main return
  exists <- run $ doesDirectoryExist dir
  assert (not exists)

projectIsValidCabal main = monadicIO $ do
  valid <- run $ mkCabalIn "hs2asttest" [] main cabalCheck
  assert valid

cabalCheck :: FilePath -> IO Bool
cabalCheck dir = do
  have <- haveCabal
  if have then do (code, sOut, sErr) <- cabal ["check"] dir ""
                  case code of
                       ExitSuccess   -> return True
                       ExitFailure c -> do print (("Exit code", c),
                                                  ("Stdout", sOut),
                                                  ("Stderr", sErr))
                                           return False
          else print "Cabal not available" >> return True

--projectRunHelloWorld = monadicIO $ do
--  run $

pureTests   = testGroup "Pure Mod2AST tests" [
    testProperty "All packages become build-depends" projectHasPkgs
  , testProperty "Can set main function" projectHasMain
  ]

impureTests = testGroup "Impure Mod2AST tests" [
    testProperty "hs2astDeps matches our Cabal file" hs2astDependenciesMatch
  , testProperty "Actions are run in the context of Cabal" actionsAreRun
  , testProperty "Temporary Cabal directory made" projectDirMade
  , testProperty "Temporary Cabal directory cleaned up" projectDirCleanedUp
  , testProperty "Temporary Cabal project is valid" projectIsValidCabal
  ]
