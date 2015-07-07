{-# LANGUAGE FlexibleInstances #-}

module HS2AST.Tests.Generators where

import Control.Applicative
import qualified Data.AttoLisp              as L
import Data.Data
import qualified Data.Set as Set
import HS2AST.Sexpr
import HS2AST.Types
import System.Directory
import Test.QuickCheck

-- Arbitrary instances

instance Arbitrary L.Lisp where
  arbitrary = let f 0 = mkLeaf <$> arbitrary
                  f n = mkNode <$> divideBetween f n
              in choose (0, 500) >>= f

-- Generator combinators

-- | Remove duplicates from a list. Will only be empty when the input is.
unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Helper functions

-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
divideBetween f 0 = return []
divideBetween f n = do size <- choose (1, abs n)
                       head <- f size
                       tail <- divideBetween f (n - size)
                       return (head : tail)
