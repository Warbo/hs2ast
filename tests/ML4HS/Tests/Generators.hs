{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}

module ML4HS.Tests.Generators (
   arbBad
 , unique
 ) where

import ML4HS.Types
import Data.Maybe
import BasicTypes
import CoreSyn
import DataCon
import qualified Data.Set as Set
import GHC
import GHC.Paths
import System.Directory
import System.IO.Unsafe
import TcEvidence
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Arbitrary instances

instance Arbitrary Haskell where
  arbitrary = return (H "f x = x")

instance Arbitrary (HsModule RdrName) where
  arbitrary = do H src <- arbitrary
                 let (Right (_, L _ x)) = parser src unsafeDynFlags "QuickCheck"
                 return x

instance Arbitrary HsFile where
  arbitrary = oneof (map return goodFiles)

-- Generator combinators

unique' lstOf gen len set | Set.size set >= len = return set
unique' lstOf gen len set | otherwise           = do
  xs <- lstOf gen
  unique' lstOf gen len (Set.union set (Set.fromList xs))

-- | Generate a list of values containing no duplicates. Note that dupes are
-- discarded, so the output length may be less than 'gen' provides.
unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Helper functions

-- Test data from disk. unsafePerformIO is justified since:
-- a) Test files should remain constant during a run
-- b) Files can't be relied on anyway, since they're only accessible via Gen
testFiles :: String -> [HsFile]
testFiles d = let paths = unsafePerformIO (getDirectoryContents d)
               in catMaybes . map (mkHs . (d ++)) $ paths

-- | Haskell files
goodFiles, badFiles :: [HsFile]
[goodFiles, badFiles] = map testFiles ["tests/data/good/", "tests/data/bad/"]

{-|
  Generate paths to invalid Haskell files, for example to test how your
  parser handles invalid input. Acts like 'arbitrary', but avoids a new type.
-}
arbBad :: Gen HsFile
arbBad = oneof (map return badFiles)

-- Runs Ghc monad in IO monad in QuickCheck monad...
go = run . runGhc (Just libdir)

-- Don't rely on these to be stable
unsafeDynFlags = unsafePerformIO (runGhc (Just libdir) getSessionDynFlags)
