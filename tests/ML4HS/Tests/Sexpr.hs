module ML4HS.Tests.Sexpr (tests) where

import Debug.Trace
import Data.Typeable
import Data.Data
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
            testProperty "Ints convert nicely"     intToSexpr
          , testProperty "Haskell converts nicely" haskellConverts
          , testProperty "Can strip from pairs"    prodStripped
          , testProperty "Can strip from sums"     sumStripped
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

-- Simplified Sexpr builders; don't perform generic type-matching stuff
sLeaf :: Data a => a -> Sexpr String
sLeaf = mkLeaf . strConstr

prodStripped :: Int -> String -> Bool
prodStripped a b = let expected = mkNode [sLeaf (a, b),
                                          mkNode [sLeaf a]]
                       result   = toSexp [typeRep [b]] [] (a, b)
                   in  result == Just expected

sumStripped :: Either Int String -> Bool
sumStripped x = let inner    = case x of
                                    Left  i -> []
                                    Right s -> [mkNode [sLeaf s]]
                    expected = Just (mkNode (sLeaf x : inner))
                    result   = toSexp [typeRep [0 :: Int]] [] x
                in  trace (show [result, expected]) $ result == expected
