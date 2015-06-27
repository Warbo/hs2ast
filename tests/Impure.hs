module Main where

import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck
import qualified HS2AST.Tests.HS2AST  as H
import qualified HS2AST.Tests.Sexpr   as S

withOptions = localOption (QuickCheckTests 1)

main = defaultMain $ withOptions $ testGroup "Impure tests" [
    H.impureTests
  , S.impureTests
  ]
