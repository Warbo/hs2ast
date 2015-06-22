module HS2AST.Tests.Parser (pureTests, impureTests) where

import Control.Exception (SomeException, bracket)
import GhcMonad
import HS2AST.Parser
import HS2AST.Types
import HS2AST.Tests.Generators
import GHC
import System.IO
import System.IO.Temp
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run, PropertyM)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck

pureTests   = testGroup "Pure parser tests" []

impureTests = testGroup "Monadic parser tests" [
                  testProperty "No empty ModuleSummary"     (ioNE noEmptyModuleSummary)
                , testProperty "Parse errors cause abort"   parseErrorsCauseAbort
                , testProperty "Bindings can be extracted"  (ioNE canGetBindings)
                ]

-- | IO test with non-empty list argument
ioNE func file = monadicIO (func file)

noEmptyModuleSummary f = do Right g <- go (graphMods f)
                            assert (not (null g))

parseErrorsCauseAbort = monadicIO $ do
  f      <- pick arbBad
  result <- go (graphMods f)
  assert $ case result of
                Left  _ -> True
                Right _ -> False

parsePath p = runInSession (fmap (not . null) (bindingsFrom p))

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
