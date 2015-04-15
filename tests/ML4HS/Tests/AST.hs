module ML4HS.Tests.AST where

import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.Tasty
import Test.Tasty.QuickCheck
import ML4HS.AST

pureTests = testGroup "Pure AST tests" [
              ]

impureTests = testGroup "Monadic AST tests" [
                  testProperty "Can parse arith.hs" canParseArith
                ]

canParseArith = monadicIO $ do [arith] <- run $ parseFiles ["tests/data/arith.hs"]
                               assert True

canAnnotateArith = monadicIO $ do ast <- getAst "tests/data/arith.hs"
                                  run $ annotateAst ast
                                  assert True
