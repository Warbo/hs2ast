{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}

module ML4HS.Tests.Generators where

import BasicTypes
import CoreSyn
import DataCon
import GHC
import GHC.Paths
import System.Directory
import System.IO.Unsafe
import TcEvidence
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Wrapper types

newtype Haskell = H String

-- Arbitrary instances

instance Arbitrary Haskell where
  arbitrary = return (H "f x = x")

instance Arbitrary (HsModule RdrName) where
  arbitrary = do H src <- arbitrary
                 let (Right (_, L _ x)) = parser src unsafeDynFlags "QuickCheck"
                 return x

--instance Arbitrary (HsModule Name) where
--  arbitrary = do depanal

-- Helper functions

-- Arbitrary instance for Haskell filenames
arbHs = let justHs    = filter (("sh." ==) . take 3 . reverse)
            testsIn d = fmap (map (d ++) . justHs) (getDirectoryContents d)
            testHs    = testsIn "tests/data/"
            paths     = unsafePerformIO testHs
        in  oneof (map return paths)

-- Runs Ghc monad in IO monad in QuickCheck monad...
go = run . runGhc (Just libdir)

-- Don't rely on these to be stable
unsafeDynFlags = unsafePerformIO (runGhc (Just libdir) getSessionDynFlags)
