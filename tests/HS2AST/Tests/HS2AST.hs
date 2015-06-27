module HS2AST.Tests.HS2AST where

import Data.Char
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
                ]

impureTests = testGroup "Monadic integration tests" []
