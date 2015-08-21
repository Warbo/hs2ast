{-# LANGUAGE OverloadedStrings #-}
module HS2AST.Tests.Sexpr where

import           Control.Applicative
import           CoreSyn
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import           DataCon (DataCon)
import           Data.Data (Data)
import           Data.Maybe
import           Data.Stringable            as S
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
import           TyCon (TyCon)
import           Type
import           Unique
import           Var

tests = testGroup "S-expression tests" [
    testProperty "Ints convert to Sexprs"            intSexprOk
  , testProperty "Vars convert to Sexprs"            varSexprOk
  , testProperty "DataCons convert to Sexprs"        dcSexprOk
  , testProperty "TyCons convert to Sexprs"          tcSexprOk
  , testProperty "Arbitrary Names can have Modules"  namesHaveMods
  , testProperty "Var names appear in Sexprs"        varNamesShown
  , testProperty "Var modules appear in Sexprs"      varModsShown
  , testProperty "Var packages appear in Sexprs"     varPkgsShown
  , testProperty "DataCon names appear in Sexprs"    dcNamesShown
  , testProperty "DataCon modules appear in Sexprs"  dcModsShown
  , testProperty "DataCon packages appear in Sexprs" dcPkgsShown
  , testProperty "TyCon names appear in Sexprs"      tcNamesShown
  , testProperty "TyCon modules appear in Sexprs"    tcModsShown
  , testProperty "TyCon packages appear in Sexprs"   tcPkgsShown
  , testProperty "Recursive generators are bounded" divideBetweenBounded
  , testProperty "Generated Sexprs have bounded size" lispLeavesBounded
  , testProperty "Can parse pretty-printed Sexprs"   parseShowInverse
  ]

-- Make sure we have no GHC panics, undefined values, etc. in our s-expressions

forceSexpr :: Data a => a -> Bool
forceSexpr x = Prelude.length (show (toSexp x)) > 0

intSexprOk :: Int -> Bool
intSexprOk = forceSexpr

varSexprOk :: Var -> Bool
varSexprOk = forceSexpr

dcSexprOk :: DataCon -> Bool
dcSexprOk = forceSexpr

tcSexprOk :: TyCon -> Bool
tcSexprOk = forceSexpr

-- Make sure some of our Arbitrary Names contain Modules, otherwise we can't
-- test the module names and package keys.
namesHaveMods = forAll (vector 100) haveMod
  where haveMod (n:ns) = isJust (nameModule_maybe n) || haveMod ns

-- Check the contents of s-expressions

-- | Generic function for checking the contents of our s-expressions,
-- | parameterised in three ways:
-- |
-- | `gen` builds Exprs which contain all of the `namedThings`. For example,
-- | given [var1, var2] it might generate Exprs like `App (Var var1) (Var var2)`
-- |
-- | `propsOf` extracts "properties" (Strings) from the names of `namedThings`.
-- | For example, given the Name 'base:Data.Bool.not' it might extract the name
-- | of the module "Data.Bool".
-- |
-- | `t` is a "tag" for properties. In the example above, we might tag the
-- | module names with "mod", to get s-expressions like ("mod" "Data.Bool")
-- |
-- | QuickCheck supplies `namedThings` and `expr`. We test that, when `expr` is
-- | turned into an s-expression, all of the tagged properties are included.
checkShown :: (NamedThing a, Data a)       =>
              ([a]    -> Gen (Expr Var))   ->
              ([Name] -> [String], String) ->
              [a]                          ->
              Property
checkShown gen (propsOf, t) namedThings = forAll (gen namedThings) propsShown
  where propsShown expr = all (toSexp expr `contains`) props
        props           = map (tag t) (propsOf names)
        names           = map getName namedThings

-- | Specialise the properties to be the name, module and package
checkNameModPkg :: (NamedThing a, Data a)  =>
                   ([a] -> Gen (Expr Var)) ->
                   [[a] -> Property]
checkNameModPkg gen = map (checkShown gen) [(map     getOccString,     "name"),
                                            (ifThere moduleName,       "mod"),
                                            (ifThere modulePackageKey, "pkg")]
  where ifThere f = map (show . f) . catMaybes . map (nameModule_maybe)

-- | Specialise the namedThings to be Vars, DataCons and TyCons
[varNamesShown, varModsShown, varPkgsShown] = checkNameModPkg exprUsingVars
[ dcNamesShown,  dcModsShown,  dcPkgsShown] = checkNameModPkg exprUsingDCs
[ tcNamesShown,  tcModsShown,  tcPkgsShown] = checkNameModPkg exprUsingTCs

-- Check we can parse pretty-printed s-expressions

divideBetweenBounded (NonNegative n) = forAll (divideBetween return n) sumToN
  where sumToN xs = sum xs == n

lispLeavesBounded x = countLeaves x <= 500

countLeaves (L.List xs) = sum (map countLeaves xs)
countLeaves _           = 1

parseShowInverse x = let bs = S.fromString . S.toString $ L.encode x
                      in case AB.eitherResult (AB.parse L.lisp bs) of
                          Left err -> error (show err)
                          Right y  -> x == y

--noDodgyChars = "\x1f\x8b"
