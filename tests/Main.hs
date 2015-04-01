module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified ML4HS.Tests.Parser as P
import qualified ML4HS.Tests.Sexpr  as S

main :: IO ()
main = defaultMain $ testGroup "All tests"
         [
           P.tests
         , S.tests
         ]
