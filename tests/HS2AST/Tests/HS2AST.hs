module HS2AST.Tests.HS2AST where

import Data.Char
import HS2AST
import HS2AST.Types
import HS2AST.Tests.Generators
import System.Directory
import System.Process
import Test.Arbitrary.Cabal
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

pureTests   = testGroup "Pure integration tests" [
                  testProperty "Collating actions works" canCollateFiles
                , testProperty "Haskell files are kept" hsKept
                , testProperty "Dodgy filenames are skipped" dodgyDropped
                , testProperty "Non-dodgy filenames are kept" nonDodgyKept
                ]

impureTests = testGroup "Monadic integration tests" [
                  testProperty "Outputs are real files" allOutputsWereGiven
                , testProperty "Real files are outputted" allGivenAreOutput
                ]

canCollateFiles ds' fs' = let size = min (length ds') (length fs')
                              ds   = take size ds'
                              fs   = take size fs'
                              args = zip ds fs
                          in  collateFiles args == (reverse ds, reverse fs)

tmpDir = "/tmp/HS2ASTTest"

withProject :: (FilePath -> PropertyM IO a) -> Project -> Property
withProject a p = monadicIO $ do path   <- run $ makeProject tmpDir p
                                 result <- a path
                                 run $ removeDirectoryRecursive path

pathsIn :: FilePath -> IO [FilePath]
pathsIn dir = fmap lines (readProcess "find" [dir] "")

allOutputsWereGiven = withProject test
  where test dir = do paths <- run $ pathsIn dir
                      out   <- run $ filesToAsts paths
                      assert (allGiven paths out)
        allGiven paths []     = True
        allGiven paths (x:xs) = x `elem` paths && allGiven paths xs

allGivenAreOutput = withProject test
  where test dir = do paths <- run $ pathsIn dir
                      out   <- run $ filesToAsts paths
                      assert (givenAll paths out)
        givenAll []     out = True
        givenAll (p:ps) out = (if fileExt p `elem` ["hs", "lhs"]
                                  then p `elem` out
                                  else True) && givenAll ps out

fileExt = reverse . takeWhile (/= '.') . reverse . map toLower

clean = map unHs . toHs . sanitise

dodgyDropped pre post = clean [pre ++ "\n" ++ post] == []

nonDodgyKept s = let s' = filter (/= '\n') s
                 in  sanitise [s'] == [s']

hsKept f = not (null (sanitise [f])) ==> forAll suffixed kept
  where suffixed = do (l, h, s) <- arbitrary :: Gen (Int, Bool, Bool)
                      fmap concat . mapM elements $
                        [[f], ["."], ["", "l", "L"], ["h", "H"], ["s", "S"]]
        kept x = clean [x] == [x]
