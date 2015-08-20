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
  , testProperty "DataCon names appear in Sexprs"    dcNamesShown
  , testProperty "DataCon modules appear in Sexprs"  dcModsShown
  , testProperty "DataCon packages appear in Sexprs" dcPkgsShown
  , testProperty "TyCon names appear in Sexprs"      tcNamesShown
  , testProperty "TyCon modules appear in Sexprs"    tcModsShown
  , testProperty "TyCon packages appear in Sexprs"   tcPkgsShown
  ]

intToSexpr :: Int -> Bool
intToSexpr i = toSx i == mkLeaf (show i)

canShowVars v = Prelude.length (show (showVar v)) > 0

namesHaveMods = forAll (vector 100) haveMod
  where haveMod  []    = False
        haveMod (n:ns) = isJust (nameModule_maybe n) || haveMod ns

-- | Generic function to check if a NamedThing appears in an Expr
checkShown :: (NamedThing a, Data a)        =>
                 ([a]    -> Gen (Expr Var)) ->
                 ([Name] -> [String])       ->
                 String                     ->
                 [a]                        ->
                 Property
checkShown g f s vs = forAll (g vs) bitsShown
  where bits        = f $ map getName vs
        bitsShown x = all (\b -> contains s b (toSexp x)) bits

checkNameModPkg gen = [
    checkShown gen (map     getOccString)     "name"
  , checkShown gen (ifThere moduleName)       "mod"
  , checkShown gen (ifThere modulePackageKey) "pkg"]

-- Check if Var names, modules and packages appear in Exprs
[varNamesShown, varModsShown, varPkgsShown] =
  checkNameModPkg exprUsingVars

-- Check if DataCon names, modules and packages appear in Exprs
[dcNamesShown, dcModsShown, dcPkgsShown] =
    checkNameModPkg exprUsingDCs

-- Check if TyCon names, modules and packages appear in Exprs
[tcNamesShown, tcModsShown, tcPkgsShown] =
  checkNameModPkg exprUsingTCs

-- Extract data from a bunch of Names, ignoring `Nothing`
ifThere f = map (show . f) . catMaybes . map (nameModule_maybe)

contains tag n x = case x of
  L.List [L.String t, L.String m] -> S.fromString tag == t &&
                                     S.fromString n   == m
  L.List xs                       -> or (map (contains tag n) xs)
  _                               -> False

noDodgyChars = "\x1f\x8b"
