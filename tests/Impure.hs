module Main where

import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck
import qualified HS2AST.Tests.Sexpr   as S

withOptions = localOption (QuickCheckTests 10)

main = defaultMain $ withOptions $ testGroup "Impure tests" [S.impureTests]
