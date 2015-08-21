{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell #-}

module HS2AST.Tests.Generators where

import           BasicTypes
import           Class
import           CoAxiom
import           Coercion
import           ConLike
import           Control.Applicative
import           CoreSyn
import qualified Data.AttoLisp              as L
import           DataCon   hiding (IntRep, FloatRep)
import           Data.Data hiding (IntRep, FloatRep)
import           Data.DeriveTH
import qualified Data.Set as Set
import           FastString
import           ForeignCall
import           HS2AST.Sexpr
import           HS2AST.Types
import           IdInfo
import MkId
import           Module
import           Name
import           OccName
import           PatSyn
import           SrcLoc
import           System.Directory
import           System.IO.Unsafe
import           Test.QuickCheck
import           TyCon
import           Type
import           UniqSupply
import           Unique
import           Var

-- Show instances, required by Arbitrary

instance Show Var where
  show = show . showVar

instance (Data a) => Show (Expr a) where
  show = show . toSexp

instance Show DataCon where
  show = show . showDataCon

instance Show TyCon.TyCon where
  show = show . showTycon

-- Arbitrary instances

-- Use this if you're making a recursive generator, so it's easier to spot
-- potential problems
recurse :: (Arbitrary a) => Gen a
recurse = arbitrary

instance Arbitrary L.Lisp where
  arbitrary = choose (0, 0) >>= genSizedLisp

genSizedLisp 0 = mkLeaf <$> arbitrary
genSizedLisp n = mkNode <$> divideBetween genSizedLisp (n - 1)

-- Enable more constructors as necessary
instance (Arbitrary a) => Arbitrary (Expr a) where
  arbitrary = frequency [(10, Var <$> arbitrary),
                         (1,  App <$> recurse   <*> arbitrary),
                         (1,  Lam <$> arbitrary <*> recurse)]

-- These cannot be derived

instance Arbitrary Var where
    arbitrary = do
        n <- arbitrary
        i <- arbitrary
        let t = mkNumLitTy i
        return (mkGlobalVar coVarDetails n t vanillaIdInfo)

instance Arbitrary ModuleName where
  arbitrary = mkModuleName <$> arbitrary

instance Arbitrary PackageKey where
  arbitrary = stringToPackageKey <$> arbitrary

instance Arbitrary Name where
    arbitrary = oneof [
      mkInternalName <$> arbitrary <*> arbitrary <*> arbitrary,
      mkExternalName <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
      mkWiredInName  <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                     <*> arbitrary,
      mkFCallName    <$> (mkUniqueGrimily <$> arbitrary) <*> arbitrary]

instance Arbitrary Unique where
  -- Morally dubious
  arbitrary = do c <- arbitrary
                 return . uniqFromSupply . unsafePerformIO . mkSplitUniqSupply $ c

instance Arbitrary OccName where
  arbitrary = mkOccName <$> arbitrary <*> arbitrary

instance Arbitrary RealSrcLoc where
  arbitrary = mkRealSrcLoc <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary RealSrcSpan where
  arbitrary = mkRealSrcSpan <$> arbitrary <*> arbitrary

instance Arbitrary FastString where
  arbitrary = fsLit <$> arbitrary

instance Arbitrary NameSpace where
  arbitrary = elements [tcName, clsName, tcClsName, dataName, OccName.varName,
                        tvName, srcDataName]

instance Arbitrary DataCon where
  arbitrary = mkDataCon <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance Arbitrary TyCon.TyCon where
  arbitrary = oneof [
    mkAlgTyCon  <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> frequency [(1, recurse), (10, return Nothing)],
    mkPrimTyCon <$> arbitrary
                <*> arbitrary
                <*> arbitrary
                <*> arbitrary]

instance Arbitrary Class where
  arbitrary = mkClass <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance Arbitrary ClassMinimalDef where
  arbitrary = undefined

instance Arbitrary Coercion where
  arbitrary = undefined

instance Arbitrary PatSyn where
  arbitrary = undefined

-- If you want to improve this, make sure your generated types make sense, ie.
-- that applications match up (eg. applying * -> * to *), etc. (GHC panics
-- otherwise). Try to keep it fast too, as they're used in a LOT of places!
instance Arbitrary Type where
  arbitrary = oneof [mkNumLitTy <$> arbitrary,
                     mkStrLitTy <$> arbitrary]

instance (Arbitrary a) => Arbitrary (UniqSM a) where
  arbitrary = undefined

instance (Arbitrary a, Arbitrary b) => Arbitrary (BranchList a b) where
  arbitrary = undefined

instance CoArbitrary Type where
  coarbitrary x = case tyConAppArgs x of
                       []     -> variant 0
                       (t:ts) -> variant 1 . coarbitrary ts

instance CoArbitrary Var where
  coarbitrary x = coarbitrary (getUnique x)

instance CoArbitrary Unique where
  coarbitrary x = variant (getKey x)

-- More-specific generators

exprUsingVars :: [Var] -> Gen (Expr Var)
exprUsingVars []     = arbitrary
exprUsingVars (v:vs) = do x <- exprUsingVars vs
                          return (App (Var v) x)

exprUsingDCs :: [DataCon] -> Gen (Expr Var)
exprUsingDCs ds = Case <$> arbitrary <*> arbitrary <*> arbitrary <*> altOf ds
  where altOf []     = return []
        altOf (d:ds) = do xs <- altOf ds
                          bs <- arbitrary
                          e  <- arbitrary
                          return ((DataAlt d, bs, e):xs)

exprUsingTCs :: [TyCon.TyCon] -> Gen (Expr Var)
exprUsingTCs ts = Type <$> typeUsing ts
  where typeUsing  []    = arbitrary
        typeUsing (t:ts) = do x <- typeUsing ts
                              return (Type.mkTyConApp t [x])

-- Generator combinators

-- | Remove duplicates from a list. Will only be empty when the input is.
unique :: (Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Helper functions

-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
divideBetween f 0 = return []
divideBetween f n = do size <- choose (1, abs n)
                       head <- f size
                       tail <- divideBetween f (n - size)
                       return (head : tail)

derive makeArbitrary ''CType
derive makeArbitrary ''Header
derive makeArbitrary ''DefMeth
derive makeArbitrary ''ClassATItem
derive makeArbitrary ''DataConRep
derive makeArbitrary ''Unbranched
derive makeArbitrary ''CoAxiom
derive makeArbitrary ''CoAxBranch
derive makeArbitrary ''RecFlag
derive makeArbitrary ''DataConBoxer
derive makeArbitrary ''Branched
derive makeArbitrary ''StrictnessMark
derive makeArbitrary ''Bind
derive makeArbitrary ''TyConParent
derive makeArbitrary ''AlgTyConRhs
derive makeArbitrary ''HsBang
derive makeArbitrary ''ConLike
derive makeArbitrary ''Role
derive makeArbitrary ''SrcSpan
derive makeArbitrary ''BuiltInSyntax
derive makeArbitrary ''TyThing
derive makeArbitrary ''Module
derive makeArbitrary ''PrimRep
derive makeArbitrary ''PrimElemRep
