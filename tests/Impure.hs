module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified HS2AST.Tests.HS2AST  as H
import qualified HS2AST.Tests.Mod2AST as M
import qualified HS2AST.Tests.Parser  as P
import qualified HS2AST.Tests.Sexpr   as S

main = defaultMain $ testGroup "Impure tests" [
    H.impureTests
  , M.impureTests
  , P.impureTests
  , S.impureTests
  ]
