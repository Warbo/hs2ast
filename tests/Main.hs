module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified ML4HS.Tests.Parser as P
import qualified ML4HS.Tests.Sexpr  as S

-- Separate slow monadic tests from pure ones, to allow toggling speed/accuracy

impureTests = testGroup "Monadic tests" [
                  S.impureTests
                , P.impureTests
                ]

pureTests   = testGroup "Pure tests" [
                  S.pureTests
                , P.pureTests
                ]

main :: IO ()
main = defaultMain $ testGroup "All tests"
         [
           pureTests
         , impureTests
         ]
