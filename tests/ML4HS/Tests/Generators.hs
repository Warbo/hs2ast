{-# LANGUAGE FlexibleInstances #-}

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
import Test.QuickCheck.Gen
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

instance Arbitrary a => Arbitrary (Sexpr a) where
  arbitrary = let f n = do x  <- arbitrary
                           xs <- divideBetween f n
                           return (Sx x xs)
              in do n <- choose (0, 500)
                    f n

-- Generator combinators

-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
divideBetween f 0 = return []
divideBetween f n = do size <- choose (1, abs n)
                       head <- f size
                       tail <- divideBetween f (n - size)
                       return (head : tail)

-- | Remove duplicates from a list. Will only be empty when the input is.
unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Helper functions

-- Test data from disk. unsafePerformIO is justified since:
-- a) Test files should remain constant during a run
-- b) Files can't be relied on anyway, since they're only accessible via Gen
testFiles :: String -> [HsFile]
testFiles d = let paths = unsafePerformIO (getDirectoryContents d)
               in mapMaybe (mkHs . (d ++)) paths

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
go = run . runInSession

-- Don't rely on these to be stable across runs
{-# NOINLINE unsafeDynFlags #-}
unsafeDynFlags = unsafePerformIO (runInSession getSessionDynFlags)
