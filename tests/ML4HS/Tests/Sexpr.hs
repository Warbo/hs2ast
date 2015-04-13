module ML4HS.Tests.Sexpr (tests) where

import Debug.Trace
import Data.Typeable
import Control.Applicative
import ML4HS.Parser
import ML4HS.Sexpr
import ML4HS.Types
import ML4HS.Tests.Generators
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

tests = testGroup "Sexpression tests"
          [
            testProperty "Ints convert nicely" intToSexpr
          , testProperty "Haskell converts nicely" haskellConverts
          --, testProperty "Can strip from pairs" prodStripped
          ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx [] [] i == Just (mkLeaf (show i))

haskellConverts h = let walk x = case unExpr x of
                                      Left  x  -> True
                                      Right xs -> all walk xs
                    in  monadicIO $ do
                          hs <- run $ runInSession (parseHaskell h)
                          case simpleAst hs of
                               Nothing  -> assert False
                               Just ast -> assert (walk ast)

{-
prodStripped :: Int -> String -> Bool
prodStripped a b = let expected = Sx (show (a, b)) [Sx (show a) []]
                       result   = strippedSexpr show [typeRep [b]] (a, b)
                   in  trace (show [result, Just expected]) $ result == Just expected
-}
