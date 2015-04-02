{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}

module ML4HS.Tests.Generators.Haskell where

import Control.Applicative
import Language.Haskell.Generate
import ML4HS.Types
import Test.QuickCheck

-- Generate Strings of Haskell code, using haskell-generate

instance Show (ExpG a) where
  show = generateExp

instance Arbitrary (ExpG Bool) where
  arbitrary = oneof [
      return true'
    , return false'
    , applyE not' <$> arbitrary
    , applyE or'  <$> arbitrary  -- Takes a *list* of arguments
    , applyE and' <$> arbitrary  -- Takes a *list* of arguments
    ]

instance Arbitrary (ExpG a) => Arbitrary (ExpG (Maybe a)) where
  arbitrary = oneof [
      return nothing'
    , applyE just' <$> arbitrary
    ]

instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) =>
          Arbitrary (ExpG (Either a b)) where
  arbitrary = oneof [
      applyE left'  <$> arbitrary
    , applyE right' <$> arbitrary
    ]

instance Arbitrary (ExpG a) => Arbitrary (ExpG [a]) where
  arbitrary = oneof [
      return $ useValue "Prelude" $ Symbol "[]"  -- Nil
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
    , applyE2 dot'   <$> arbitrary <*> arbitrary
    ]

mkNames :: [String] -> Generate [Name]
mkNames = mapM newName

instance Arbitrary Haskell where
  -- Add to this to increase confidence
  arbitrary = return (H "f x = x")

-- | Bind 'body' to 'name', and if 'keep' then add it to 'spec' (if any)
addMaybeExport :: Name -> ExpG t -> Bool -> Maybe [ExportSpec] -> ModuleG
addMaybeExport name body keep specs = do
  ref <- addDecl name body
  return $ if keep
              then fmap (exportFun ref :) specs  -- fmap from Maybe
              else specs

data Genable = forall a. (Arbitrary (ExpG a)) => G a

-- | Generate definitions for Names and choose whether to export them
mkMaybeExport :: [Name] -> Maybe [ExportSpec] -> Gen ModuleG
mkMaybeExport []     specs = return (return specs)
mkMaybeExport (n:ns) specs = do
  G body <- arbitrary
  keep <- arbitrary
  let specs' = addMaybeExport n body keep specs
  mkMaybeExport ns specs'

instance Arbitrary ModuleG where
  arbitrary = do
    -- Generate names all at once (to avoid clashes)
    (names, mods)  <- runGenerate . mkNames <$> arbitrary

    -- Choose whether to use an export list
    exports <- elements [Nothing, Just []]

    -- Generate bodies for then names
    mkMaybeExport names exports
