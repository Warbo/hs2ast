module ML4HS.Tests.Parser where

import GhcMonad
import ML4HS.Parser
import ML4HS.Tests.Generators
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Parser Tests" [
          testGroup "Monadic QuickCheck" [

            testProperty "No empty ModuleSummary" $
              monadicIO $ do m <- pick arbHs
                             g <- run $ runGhcM $ inDefaultEnv (`graphMods` [m])
                             assert (not (null g))

          , testProperty "Parse errors cause abort" $
              monadicIO $ do m <- pick arbitrary
          ]
        ]
