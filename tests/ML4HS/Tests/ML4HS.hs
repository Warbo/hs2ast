module ML4HS.Tests.ML4HS where

import ML4HS
import ML4HS.Types
import ML4HS.Tests.Generators
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

pureTests   = testGroup "Pure integration tests" []
impureTests = testGroup "Monadic integration tests" [
                  testProperty "Good files get ASTs" goodAsts
                ]

goodAsts (NonEmpty fs) = monadicIO $ do
                           asts <- run $ runInSession $ getAsts (unique fs)
                           run $ print asts
                           assert True
