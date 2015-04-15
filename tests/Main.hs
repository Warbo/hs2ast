module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified ML4HS.Tests.Parser as P
import qualified ML4HS.Tests.Sexpr  as S
import qualified ML4HS.Tests.AST    as A

-- Separate slow monadic tests from pure ones, to allow toggling speed/accuracy

impureTests = testGroup "Monadic tests" [
                  S.impureTests
                , P.impureTests
                , A.impureTests
                ]

pureTests   = testGroup "Pure tests" [
                  S.pureTests
                , P.pureTests
                , A.pureTests
                ]

main :: IO ()
main = defaultMain $ testGroup "All tests"
         [
           pureTests
         , impureTests
         ]
