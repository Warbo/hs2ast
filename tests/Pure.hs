module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified HS2AST.Tests.HS2AST  as H
import qualified HS2AST.Tests.Sexpr   as S

main = defaultMain $ testGroup "Pure tests" [
    H.pureTests
  , S.pureTests
  ]
