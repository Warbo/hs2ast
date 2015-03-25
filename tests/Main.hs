{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Test.Tasty (defaultMain, testGroup)
import qualified ML4HS.Tests.Parser as P

main :: IO ()
main = defaultMain $
  testGroup "Tests"
  [
    P.tests
  ]
