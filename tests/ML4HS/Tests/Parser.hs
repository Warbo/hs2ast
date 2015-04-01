module ML4HS.Tests.Parser (tests) where

import Control.Exception (SomeException)
import GhcMonad
import ML4HS.Parser
import ML4HS.Types
import ML4HS.Tests.Generators
import GHC
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Parser Tests" [
          testGroup "Monadic QuickCheck" [
            testProperty "No empty ModuleSummary"    (ioNE noEmptyModuleSummary)
          , testProperty "Parse errors cause abort"  parseErrorsCauseAbort
          , testProperty "Bindings can be extracted" (ioNE canGetBindings)
          ]
        ]

-- | IO test with non-empty list argument
ioNE f (NonEmpty fs) = monadicIO (f (unique fs))

noEmptyModuleSummary fs = do Right g <- go (graphMods fs)
                             assert (not (null g))

parseErrorsCauseAbort = monadicIO $ do
  fs     <- pick (listOf1 arbBad)
  result <- go (graphMods fs)
  assert $ case result of
                Left  _ -> True
                Right _ -> False

canGetBindings fs = do result <- go (bindingsFrom fs)
                       case result of
                            Right xs -> assert (not (null xs))
                            Left  e  -> showErr e

-- | Run Ghc-wrapped computations in QuickCheck
go :: Ghc a -> PropertyM IO (Either SomeException a)
go = run . sumExcept . runInSession

showErr e = liftIO (putStrLn "\nERROR:" >>
                    print e           >>
                    putStrLn "/ERROR") >> assert False

-- | Represent exceptions using a sum type
sumExcept :: IO a -> IO (Either SomeException a)
sumExcept f = protect Left (fmap Right f)
