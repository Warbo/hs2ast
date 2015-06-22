module HS2AST.Tests.Sexpr (impureTests, pureTests) where

import Debug.Trace
import Data.Typeable
import Data.Data
import Control.Applicative
import HS2AST.Parser
import HS2AST.Sexpr
import HS2AST.Types
import HS2AST.Tests.Generators
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

impureTests = testGroup "Monadic s-expression tests" [
              ]
pureTests   = testGroup "Pure s-expression tests" [
                  testProperty "Ints convert nicely" intToSexpr
                , testProperty "Can strip first"     prodStripL
                , testProperty "Can strip second"    prodStripR
                , testProperty "Can keep first"      prodKeepL
                , testProperty "Can keep second"     prodKeepR
                , testProperty "Can strip left"      sumStripL
                , testProperty "Can strip right"     sumStripR
                , testProperty "Can keep left"       sumKeepL
                , testProperty "Can keep right"      sumKeepR
                ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx [] [] i == Just (mkLeaf (show i))

-- Simplified Sexpr builders; don't perform generic type-matching stuff
sLeaf :: Data a => a -> Sexpr String
sLeaf = mkLeaf . strConstr

sList :: Data a => [a] -> Sexpr String
sList l@[]     = sLeaf l
sList l@(x:xs) = mkNode [sLeaf l, sList xs]

anyTrue = anysx ("True" ==)

trB = typeRep [True]

prodStrip :: (Data a, Data b) => a -> b -> Bool
prodStrip a b = let Just result = toSexp [trB] [] (a, b)
                in  not (anyTrue result)

prodKeep :: (Data a, Data b) => a -> b -> Bool
prodKeep a b = let Just result = toSexp [] [] (a, b)
                   in anyTrue result

prodStripL, prodStripR :: Bool -> String -> Bool
prodStripL a b = prodStrip a b
prodStripR a b = prodStrip b a

prodKeepL, prodKeepR :: String -> Bool -> Bool
prodKeepL a b = prodKeep a b == b
prodKeepR a b = prodKeep b a == b

sumStripL :: Either Bool String -> Bool
sumStripL x = let Just result = toSexp [trB] [] x
              in  not (anyTrue result)

sumStripR :: Either String Bool -> Bool
sumStripR x = let Just result = toSexp [trB] [] x
              in  not (anyTrue result)

sumKeepL :: Either Bool String -> Bool
sumKeepL x = let Just result = toSexp [] [] x
             in  anyTrue result == (x == Left True)

sumKeepR :: Either String Bool -> Bool
sumKeepR x = let Just result = toSexp [] [] x
             in  anyTrue result == (x == Right True)

-- | Fold an Sexpr
foldsx :: (a -> b) -> ([b] -> b) -> Sexpr a -> b
foldsx leaf node sx = case unExpr sx of
  Left  x  -> leaf x
  Right xs -> node (map (foldsx leaf node) xs)

anysx :: (a -> Bool) -> Sexpr a -> Bool
anysx f = foldsx f (any id)
