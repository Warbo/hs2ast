{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ML4HS.Tests.Generators.Haskell where

import Control.Applicative
import Data.Maybe
import Language.Haskell.Generate
import ML4HS.Types
import Test.QuickCheck

-- Generate Strings of Haskell code, using haskell-generate
-- Very incomplete; feel free to add more combinations!

-- | Generate a Haskell module, render it to a String then wrap in 'H'
instance Arbitrary Haskell where
  arbitrary = do fmap (H . show) (arbitrary :: Gen ModuleG)

instance Show (ExpG a) where
  show = generateExp

-- | We only generate "Main" modules for now, to avoid dependency problems
instance Show ModuleG where
  show m = generateModule m "Main"

-- | Haskell expressions need a type, which we must reify to avoid ambiguity
data TYPE a b = PROD (ExpG (a, b))        -- ^ Product of 'a' and 'b'
              | SUM  (ExpG (Either a b))  -- ^ Sum of 'a' and 'b'
              | EXP  (ExpG (a -> b))      -- ^ Function types
              | LIST (ExpG [a])           -- ^ Lists (we ignore 'b')
              | SWAP (TYPE b a)           -- ^ Switch the order of arguments
              | UNIT (TYPE a ())
              | INT  (TYPE a Int)
              | IOT  (TYPE (IO a) b)

-- | If we can generate expressions of 'a' and 'b', we can generate a 'TYPE a b'
instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) => Arbitrary (TYPE a b) where
  arbitrary = do n <- choose (0, 7)
                 case (n :: Int) of
                      0 -> PROD <$> arbitrary
                      1 -> SUM  <$> arbitrary
                      2 -> EXP  <$> arbitrary
                      3 -> LIST <$> arbitrary
                      4 -> SWAP <$> arbitrary
                      5 -> UNIT <$> arbitrary
                      6 -> INT  <$> arbitrary
                      7 -> IOT  <$> arbitrary

-- | Extract an 'ExpG' from a 'TYPE' and pass to 'meb'. This avoids intermediate
-- types, which would complicate things.
tToE :: TYPE t1 t2 -> Bool -> Name -> ModuleM [ExportSpec]
tToE t = case t of
  PROD e -> meb  e
  SUM  e -> meb  e
  EXP  e -> meb  e
  LIST e -> meb  e
  SWAP e -> tToE e
  UNIT e -> tToE e
  INT  e -> tToE e
  IOT  e -> tToE e

-- Arbitrary expressions ('ExpG a')

instance Arbitrary (ExpG ()) where
  arbitrary = oneof [
      return tuple0
    ]

instance Arbitrary (ExpG Bool) where
  arbitrary = oneof [
      return true'
    , return false'
    , applyE not' <$> arbitrary
    , applyE or'  <$> arbitrary  -- Takes a *list* of arguments
    , applyE and' <$> arbitrary  -- Takes a *list* of arguments
    ]

instance Arbitrary (ExpG Int) where
  arbitrary = do x <- arbitrary
                 return . fromInteger . abs $ x

instance Arbitrary (ExpG a) => Arbitrary (ExpG (Maybe a)) where
  arbitrary = oneof [
      return nothing'
    , applyE just' <$> arbitrary
    ]

instance Arbitrary (ExpG a) => Arbitrary (ExpG (IO a)) where
  arbitrary = oneof [
      return undefined'
    ]

instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) =>
          Arbitrary (ExpG (Either a b)) where
  arbitrary = oneof [
      applyE left'  <$> arbitrary
    , applyE right' <$> arbitrary
    ]

instance Arbitrary (ExpG a) => Arbitrary (ExpG [a]) where
  arbitrary = oneof [
      return undefined'
    , applyE2 cons <$> arbitrary <*> arbitrary
    ]

instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) =>
          Arbitrary (ExpG (a, b)) where
  arbitrary = oneof [
      applyE2 tuple2 <$> arbitrary <*> arbitrary
    ]

instance Arbitrary (ExpG b) => Arbitrary (ExpG (a -> b)) where
  arbitrary = oneof [
      applyE  const' <$> arbitrary
    ]

-- | Bind 'body' to 'name', and if 'keep' then add it to 'spec' (if any)
addMaybeExport :: Name -> ExpG t -> Bool -> Maybe [ExportSpec] -> ModuleG
addMaybeExport name body keep specs = do
  ref <- addDecl name body
  return $ if keep
              then fmap (exportFun ref :) specs  -- fmap from Maybe
              else specs

meb :: ExpG t -> Bool -> Name -> ModuleM [ExportSpec]
meb body keep name = do ref <- addDecl name body
                        if keep then return [exportFun ref]
                                else return []

-- | Generate definitions for Names and choose whether to export them
mkMaybeExport :: [Name] -> Gen (ModuleM [ExportSpec])
mkMaybeExport []     = return (return [])
mkMaybeExport (n:ns) = do
  body  <- arbitrary :: Gen (TYPE Int Bool)
  let body' = tToE body
  specs <- mkMaybeExport ns
  keep  <- arbitrary
  return ((++) <$> (body' keep n) <*> specs)

mme :: [Name] -> Gen ModuleG
mme ns = do specs <- mkMaybeExport ns
            return (fmap Just specs)

mkMain :: ExpG (IO ()) -> ModuleG -> ModuleG
mkMain body x = do x'  <- fromMaybe [] <$> x
                   ref <- addDecl (Ident "main") body
                   return (Just (exportFun ref : x'))

genName :: Gen String
genName = do c <- choose ('a', 'z')
             cs <- listOf $ oneof [choose ('a', 'z'),
                                   choose ('A', 'Z'),
                                   choose ('0', '9'),
                                   elements ['_', '\'']]
             return (c:cs)

instance Arbitrary ModuleG where
  arbitrary = do
    -- Generate names all at once (to avoid clashes)
    (names, mods)  <- runGenerate . mapM newName <$> listOf genName

    -- Generate bodies for the names
    specs <- mme names

    -- Generate a "main" value
    main <- arbitrary

    return (mkMain main specs)
