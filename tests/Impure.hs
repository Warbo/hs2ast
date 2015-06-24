module Main where

import           Test.Tasty (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck
import qualified HS2AST.Tests.HS2AST  as H
import qualified HS2AST.Tests.Mod2AST as M
import qualified HS2AST.Tests.Parser  as P
import qualified HS2AST.Tests.Sexpr   as S

withOptions = id localOption (QuickCheckTests 10)

main = defaultMain $ withOptions $ testGroup "Impure tests" [
    H.impureTests
  , M.impureTests
  , P.impureTests
  , S.impureTests
  ]
