{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ML4HS.Tests.Generators.Haskell where

import Control.Applicative
import Language.Haskell.Generate
import ML4HS.Types
import Test.QuickCheck

-- Generate Strings of Haskell code, using haskell-generate

instance Arbitrary Haskell where
  -- Add to this to increase confidence
  arbitrary = do fmap (H . show) (arbitrary :: Gen ModuleG)

instance Show (ExpG a) where
  show = generateExp

instance Show ModuleG where
  show m = generateModule m "Main"

data TYPE a b = PROD (ExpG (a, b))
              | SUM  (ExpG (Either a b))
              | EXP  (ExpG (a -> b))
              | LIST (ExpG [a])
              | SWAP (TYPE b a)

instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) => Arbitrary (TYPE a b) where
  arbitrary = do n <- choose (0, 4)
                 case (n :: Int) of
                      0 -> PROD <$> arbitrary
                      1 -> SUM  <$> arbitrary
                      2 -> EXP  <$> arbitrary
                      3 -> LIST <$> arbitrary
                      4 -> SWAP <$> arbitrary

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

mapT :: TYPE t1 t2 -> Bool -> Name -> ModuleM [ExportSpec]
mapT t = case t of
  PROD e -> meb  e
  SUM  e -> meb  e
  EXP  e -> meb  e
  LIST e -> meb  e
  SWAP e -> mapT e

-- | Generate definitions for Names and choose whether to export them
mkMaybeExport :: [Name] -> Gen (ModuleM [ExportSpec])
mkMaybeExport []     = return (return [])
mkMaybeExport (n:ns) = do
  body  <- arbitrary :: Gen (TYPE Int Bool)
  let body' = mapT body
  specs <- mkMaybeExport ns
  keep  <- arbitrary
  return ((++) <$> (body' keep n) <*> specs)

mme :: [Name] -> Gen ModuleG
mme ns = do specs <- mkMaybeExport ns
            return (fmap Just specs)

mkMain :: ModuleG -> ModuleG
mkMain x = do x'  <- x
              ref <- addDecl (Ident "main") undefined'
              let ref' = exportFun ref
              case x' of
                   Nothing  -> return (Just [ref'])
                   Just x'' -> return (Just (ref':x''))

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

    -- Generate bodies for then names
    specs <- mme names

    --export <- arbitrary
    --if export then do
    return (mkMain specs)
    --          else do return (mkMain (return Nothing))
