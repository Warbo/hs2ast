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
  , testProperty "Var names appear in Sexprs"        varNamesShown
  , testProperty "Var modules appear in Sexprs"      varModsShown
  , testProperty "Var packages appear in Sexprs"     varPkgsShown
  ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx i == mkLeaf (show i)

canShowVars v = Prelude.length (show (showVar v)) > 0

namesHaveMods = forAll (vector 100) haveMod
  where haveMod  []    = False
        haveMod (n:ns) = isJust (nameModule_maybe n) || haveMod ns

varNamesShown :: [Var] -> Property
varNamesShown vs = forAll (exprUsing vs) namesShown
  where names        = map getN vs
        getN         = S.fromString . getOccString . getName
        namesShown x = all (\n -> contains "name" n (toSexp x)) names

varCheckShown f s vs = forAll (exprUsing vs) bitsShown
  where bits        = catMaybes (map getBit vs)
        getBit      = fmap (S.fromString . show . f) . nameModule_maybe . getName
        bitsShown x = all (\b -> contains s b (toSexp x)) bits

varModsShown = varCheckShown moduleName       "mod"
varPkgsShown = varCheckShown modulePackageKey "pkg"

contains tag n x = case x of
  L.List [L.String tag, L.String m] -> n == m
  L.List xs                         -> or (map (contains tag n) xs)
  _                                 -> False

noDodgyChars = "\x1f\x8b"
