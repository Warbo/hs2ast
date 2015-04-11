module ML4HS.Tests.Parser (tests) where

import Control.Exception (SomeException, bracket)
import GhcMonad
import ML4HS.Parser
import ML4HS.Types
import ML4HS.Tests.Generators
import GHC
import System.IO
import System.IO.Temp
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Parser Tests" [
          testGroup "Monadic QuickCheck" [
            testProperty "No empty ModuleSummary"     (ioNE noEmptyModuleSummary)
          , testProperty "Parse errors cause abort"   parseErrorsCauseAbort
          , testProperty "Bindings can be extracted"  (ioNE canGetBindings)
          , testProperty "Generated code is parsable" genCodeWillParse
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

parsePath p = runInSession (fmap (not . null) (bindingsFrom [p]))

canGetBindings fs = do result <- go (bindingsFrom fs)
                       case result of
                            Right xs -> assert (not (null xs))
                            Left  e  -> showErr e

-- | Run Ghc-wrapped computations in QuickCheck
go :: Ghc a -> PropertyM IO (Either SomeException a)
go = run . sumExcept . runInSession

showErr e = liftIO (putStrLn "\nERROR:" >>
                    print e             >>
                    putStrLn "/ERROR")  >> assert False

-- | Represent exceptions using a sum type
sumExcept :: IO a -> IO (Either SomeException a)
sumExcept f = protect Left (fmap Right f)

withHaskellFile f h = do result <- run $ withTempHaskell h (sumExcept . f)
                         case result of
                              Right b -> assert  b
                              Left  e -> showErr e

genCodeWillParse h = monadicIO $ withHaskellFile parsePath h
