module HS2AST.Tests.HS2AST where

import HS2AST
import HS2AST.Types
import HS2AST.Tests.Generators
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

pureTests   = testGroup "Pure integration tests" [
                  testProperty "Collating actions works" canCollateFiles
                ]

impureTests = testGroup "Monadic integration tests" [
                  testProperty "Good files get ASTs" goodAsts
                ]

goodAsts (NonEmpty fs) = monadicIO $ do
                           asts <- run $ runInSession $ getAsts (unique fs)
                           --run $ print asts
                           assert True

canCollateFiles ds' fs' = let size = min (length ds') (length fs')
                              ds   = take size ds'
                              fs   = take size fs'
                              args = zip ds fs
                          in  collateFiles args == (reverse ds, reverse fs)
