{-# LANGUAGE FlexibleInstances, FlexibleContexts, ExistentialQuantification #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module ML4HS.Tests.Generators.Haskell where

import Debug.Trace
import Control.Applicative
import Data.Maybe
import Language.Haskell.Generate
import ML4HS.Types
import Test.QuickCheck

-- Generate Haskell code by pretty-printing generated Haskell modules, created
-- using haskell-generate.
-- Very incomplete; feel free to add more combinations!

-- | Generate a Haskell module, render it to a String then wrap in 'H'
instance Arbitrary Haskell where
  arbitrary = do fmap (H . show) (arbitrary :: Gen ModuleG)

-- A module contains any number of definitions, each with a name and a body.
-- We only generate Main modules, to keep GHC happy.

-- | Generate Haskell modules
instance Arbitrary ModuleG where
  arbitrary = do
    -- Generate names for our definitions all at once (to avoid clashes)
    n             <- small
    (names, mods) <- runGenerate . mapM newName . take n <$> listOf genName

    -- Generate bodies for the names
    specs <- do specs <- mkMaybeExport names
                return (fmap Just specs)

    -- Generate a "main" value (to appease GHC)
    main <- arbitrary

    return (mkMain main specs)

-- | We only generate "Main" modules for now, to avoid dependency problems
instance Show ModuleG where
  show m = generateModule m "Main"

-- Terms are recursive trees, so we need to be careful that the expected number
-- of recursive calls is less than one. We do this using 'ArbSize', which gives
-- all of our generators a decreasing "size" parameter. We induce Arbitrary
-- instances by providing a small size.

class ArbSize a where
  arb :: Int -> Gen a

instance (ArbSize (ExpG a)) => Arbitrary (ExpG a) where
  arbitrary = small >>= arb

small :: Gen Int
small = (`mod` 10) . abs <$> arbitrary

instance Show (ExpG a) where
  show = generateExp

-- Generating a term depends on its type (except for undefined'); for example,
-- to generate a term of type @(a, b)@ we might generate terms of type @a@ and
-- @b@, or we might use function calls which return an @(a, b)@. Hence we write
-- multiple Arbitrary implementations, to cover a few interesting types.

instance ArbSize (ExpG ()) where
  arb _ = return tuple0

instance ArbSize (ExpG Bool) where
  arb n | n < 0 = error ("arb " ++ show n ++ " :: Gen Bool")
  arb 0 = oneof [return true', return false']
  arb n = oneof [
              arb 0
            , applyE not' <$> arb (n - 1)
            , applyE or'  <$> arb (n - 1)  -- Takes a *list* of arguments
            , applyE and' <$> arb (n - 1)  -- Takes a *list* of arguments
            ]

instance ArbSize (ExpG Int) where
  arb n = do x <- choose (0, n)
             return . fromIntegral . abs $ x

instance ArbSize (ExpG a) => ArbSize (ExpG (Maybe a)) where
  arb n | n < 0 = error ("arb " ++ show n ++ " :: Gen Maybe")
  arb 0 = return nothing'
  arb n = oneof [
      return nothing'
    , applyE just' <$> arb (n-1)
    ]

instance ArbSize (ExpG a) => ArbSize (ExpG (IO a)) where
  arb _ = return undefined'

instance (ArbSize (ExpG a), ArbSize (ExpG b)) =>
          ArbSize (ExpG (Either a b)) where
  arb n | n < 0 = error ("arb " ++ show n ++ " :: Gen Either")
  arb n = let m = max 0 (n - 1)
          in  oneof [applyE left'  <$> arb m,
                     applyE right' <$> arb m]

nil = undefined'  -- Prelude.[] seems to cause parse errors

instance ArbSize (ExpG a) => ArbSize (ExpG [a]) where
  arb n | n < 0 = error ("arb " ++ show n ++ " :: Gen List")
  arb 0 = return nil
  arb n = let collate []     = nil
              collate (x:xs) = applyE2 cons x (collate xs)
          in  collate <$> divBet (n - 1)

instance (ArbSize (ExpG a), ArbSize (ExpG b)) =>
          ArbSize (ExpG (a, b)) where
  arb n | n < 0 = error ("arb " ++ show n ++ " :: Gen Pair")
  arb 0 = applyE2 tuple2 <$> arb 0 <*> arb 0
  arb n = do size <- choose (1, n)
             l    <- arb size
             r    <- arb (n - size)
             return (applyE2 tuple2 l r)

instance ArbSize (ExpG b) => ArbSize (ExpG (a -> b)) where
  arb n = let m = max 0 (n - 1)
          in  applyE const' <$> arb m

-- Since we're generating a raw String, we don't know what types to give our
-- definitions. We solve this by making a concrete representation for types
-- ('TYPE'), generating Arbitrary type representations, then choosing a term
-- generator based on these representations.

-- | Haskell expressions need a type, which we must reify to avoid ambiguity
data TYPE a b = PROD (ExpG (a, b))        -- ^ Product of 'a' and 'b'
              | SUM  (ExpG (Either a b))  -- ^ Sum of 'a' and 'b'
              | EXP  (ExpG (a -> b))      -- ^ Function types
              | LIST (ExpG [a])           -- ^ Lists (we ignore 'b')
              | SWAP (TYPE b a)           -- ^ Switch the order of arguments

-- | If we can generate expressions of 'a' and 'b', we can generate a 'TYPE a b'
instance (ArbSize (ExpG a), ArbSize (ExpG b)) => ArbSize (TYPE a b) where
  arb n = let m = max 0 (n - 1)
          in  oneof [
                  PROD <$> arb m
                , SUM  <$> arb m
                , EXP  <$> arb m
                , LIST <$> arb m
                , SWAP <$> arb m
                ]

instance ArbSize (TYPE a b) => Arbitrary (TYPE a b) where
  arbitrary = small >>= arb

-- | Add any type of expression to a module
meb :: ExpG t -> Bool -> Name -> ModuleM [ExportSpec]
meb body keep name = do ref <- addDecl name body
                        if keep then return [exportFun ref]
                                else return []

-- | Extract an 'ExpG' from a 'TYPE' and pass to 'meb'. This avoids intermediate
-- types, which would complicate things.
tToE :: TYPE t1 t2 -> Bool -> Name -> ModuleM [ExportSpec]
tToE t = case t of
              PROD e -> meb  e
              SUM  e -> meb  e
              EXP  e -> meb  e
              LIST e -> meb  e
              SWAP e -> tToE e

-- | Generate definitions for Names and choose whether to export them
mkMaybeExport :: [Name] -> Gen (ModuleM [ExportSpec])
mkMaybeExport []     = return (return [])
mkMaybeExport (n:ns) = do
  body <- arbitrary :: Gen (TYPE Int Bool)
  let body' = tToE body
  specs <- mkMaybeExport ns
  keep  <- arbitrary
  return ((++) <$> (body' keep n) <*> specs)

mkMain :: ExpG (IO ()) -> ModuleG -> ModuleG
mkMain body x = do x'  <- fromMaybe [] <$> x
                   ref <- addDecl (Ident "main") body
                   return (Just (exportFun ref : x'))

genName :: Gen String
genName = do c  <- choose ('a', 'z')
             cs <- listOf $ oneof [choose ('a', 'z'),
                                   choose ('A', 'Z'),
                                   choose ('0', '9'),
                                   elements ['_', '\'']]
             return (c:cs)

divBet :: ArbSize a => Int -> Gen [a]
divBet n | n < 0 = error ("divBet " ++ show n)
divBet 0 = return []
divBet n = do size <- choose (1, n)
              head <- arb size
              tail <- divBet (n - size)
              return (head : tail)
