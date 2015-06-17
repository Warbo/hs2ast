{-# LANGUAGE FlexibleInstances #-}

module HS2AST.Tests.Generators (
   arbBad
 , unique
 , gen
 ) where

import HS2AST.Types
import Control.Applicative
import Data.Maybe
import Data.Data
import qualified Data.Set as Set
import GHC
import System.Directory
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

-- Arbitrary instances

instance Arbitrary HsFile where
  arbitrary = oneof (map return goodFiles)

instance (Data a, Arbitrary a) => Arbitrary (Sexpr a) where
  arbitrary = let f 0 = mkLeaf <$> arbitrary
                  f n = mkNode <$> divideBetween f n
              in choose (0, 500) >>= f

-- Generator combinators

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

-- | Runs Ghc monad in IO monad in QuickCheck monad...
go = run . runInSession

-- | Makes debugging easier
gen n = let rounds = ceiling (fromIntegral n / 11.0)
            chunks = vectorOf rounds arbitrary
        in do samples <- sample' chunks
              return (take n (concat samples))

-- Don't rely on this to be stable across runs
{-# NOINLINE unsafeDynFlags #-}
unsafeDynFlags = unsafePerformIO (runInSession getSessionDynFlags)

-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
divideBetween f 0 = return []
divideBetween f n = do size <- choose (1, abs n)
                       head <- f size
                       tail <- divideBetween f (n - size)
                       return (head : tail)
