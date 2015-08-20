{-# LANGUAGE OverloadedStrings #-}
module HS2AST.Tests.Sexpr (impureTests, pureTests) where

import           Control.Applicative
import           CoreSyn
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
                , testProperty "Names are shown"     namesAreShown
                ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx i == mkLeaf (show i)

canShowVars v = Prelude.length (show (showVar v)) > 0

namesAreShown :: [Var] -> Property
namesAreShown vs = forAll (exprUsing vs) namesShown
  where namesShown x = all (toSexp x `contains`) (map varNameT vs)
        varNameT = S.fromString . show . Var.varName
        contains x n = case x of
          L.List [L.String "name", L.String m] -> n == m
          L.List xs                            -> or (map (`contains` n) xs)
          _                                    -> False

noDodgyChars = "\x1f\x8b"
