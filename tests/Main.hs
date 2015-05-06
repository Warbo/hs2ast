module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified HS2AST.Tests.HS2AST  as M
import qualified HS2AST.Tests.Parser as P
import qualified HS2AST.Tests.Sexpr  as S

-- Separate slow monadic tests from pure ones, to allow toggling speed/accuracy

impureTests = testGroup "Monadic tests" [
                --  S.impureTests
                --, P.impureTests
                {-,-} M.impureTests
                ]

pureTests   = testGroup "Pure tests" [
                  S.pureTests
                , P.pureTests
                , M.pureTests
                ]

main :: IO ()
main = defaultMain $ testGroup "All tests"
         [
           pureTests
         , impureTests
         ]
