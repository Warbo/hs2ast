{-# LANGUAGE OverloadedStrings #-}
module HS2AST.Tests.Sexpr (impureTests, pureTests) where

import           Control.Applicative
import           CoreSyn
import qualified Data.AttoLisp           as L
import           Data.Data
import           Data.Maybe
import           Data.Stringable         as S
import           Data.Typeable
import           Debug.Trace
import           HS2AST.Sexpr
import           HS2AST.Tests.Generators
import           HS2AST.Types
import           IdInfo
import           Module
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
    testProperty "Ints convert to Sexprs"            intToSexpr
  , testProperty "Can show Vars"                     canShowVars
  , testProperty "Some generated Names have Modules" namesHaveMods
  , testProperty "Var names appear in Sexprs"        namesAreShown
  , testProperty "Var modules appear in Sexprs"      modsAreShown
  ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx i == mkLeaf (show i)

canShowVars v = Prelude.length (show (showVar v)) > 0

namesHaveMods = forAll (vector 100) haveMod
  where haveMod  []    = False
        haveMod (n:ns) = isJust (nameModule_maybe n) || haveMod ns

namesAreShown :: [Var] -> Property
namesAreShown vs = forAll (exprUsing vs) namesShown
  where names        = map getName vs
        getName      = S.fromString . show . Var.varName
        namesShown x = all (toSexp x `contains`) names
        contains x n = case x of
          L.List [L.String "name", L.String m] -> n == m
          L.List xs                            -> or (map (`contains` n) xs)
          _                                    -> False

modsAreShown vs = forAll (exprUsing vs) modsShown
  where mods         = catMaybes (map getMod vs)
        getMod       = fmap (S.fromString . show . moduleName) . nameModule_maybe . Var.varName
        mkMod        = tag "mod"
        modsShown x  = all (toSexp x `contains`) mods
        contains x n = case x of
          L.List [L.String "mod", L.String m] -> n == m
          L.List xs                           -> or (map (`contains` n) xs)
          _                                   -> False

tag t x = mkNode [mkLeaf t, mkLeaf x]

noDodgyChars = "\x1f\x8b"
