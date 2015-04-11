module ML4HS.Tests.Sexpr (tests) where

import ML4HS.Sexpr
import ML4HS.Types
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Sexpression tests"
          [
            testProperty "Ints convert nicely" intToSexpr
          ]

intToSexpr :: Int -> Bool
intToSexpr i = toSs i == Sx (show i) []

haskellConverts :: Haskell -> Bool
haskellConverts (H x) =
