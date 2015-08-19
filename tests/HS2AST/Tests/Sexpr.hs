module HS2AST.Tests.Sexpr (impureTests, pureTests) where

import           Control.Applicative
import qualified Data.AttoLisp           as L
import           Data.Data
import           Data.Stringable         as S
import           Data.Typeable
import           Debug.Trace
import           HS2AST.Sexpr
import           HS2AST.Tests.Generators
import           HS2AST.Types
import           IdInfo
import           Name
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Type
import           Unique
import           Var

impureTests = testGroup "Monadic s-expression tests" [
              ]
pureTests   = testGroup "Pure s-expression tests" [
                  testProperty "Ints convert nicely" intToSexpr
                , testProperty "Can show Vars"       canShowVars
                ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx i == Just (mkLeaf (show i))

canShowVars v = Prelude.length (show (showVar v)) > 0

-- Simplified Sexpr builders; don't perform generic type-matching stuff
sLeaf :: Data a => a -> L.Lisp
sLeaf = strConstr

sList :: Data a => [a] -> L.Lisp
sList l@[]     = sLeaf l
sList l@(x:xs) = mkNode [sLeaf l, sList xs]

anyTrue = anysx ("True" ==)


-- | Fold an Sexpr
foldsx :: (String -> b) -> ([b] -> b) -> L.Lisp -> b
foldsx leaf node sx = case sx of
  L.String x  -> leaf (S.toString x)
  L.List   xs -> node (map (foldsx leaf node) xs)

anysx :: (String -> Bool) -> L.Lisp -> Bool
anysx f = foldsx f or

instance Show Var where
  show = show . showVar

instance Arbitrary Name where
    arbitrary = do
      n <- arbitrary
      s <- arbitrary
      return $ mkFCallName (mkUniqueGrimily n) s

instance Arbitrary Var where
    arbitrary = do
        n <- arbitrary
        i <- arbitrary
        let t = mkNumLitTy i
        return (mkGlobalVar coVarDetails n t vanillaIdInfo)
