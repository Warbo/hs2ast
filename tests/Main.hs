module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified HS2AST.Tests.Sexpr as S

main = defaultMain $ testGroup "All tests" [S.tests]
