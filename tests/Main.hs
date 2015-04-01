module Main where

import           Test.Tasty (defaultMain)
import qualified ML4HS.Tests.Parser as P

main :: IO ()
main = defaultMain P.tests
