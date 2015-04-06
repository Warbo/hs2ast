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

{-
data Typ = TP Typ Typ
         | TS Typ Typ
         | TE Typ Typ
         | TL Typ
         | TC
         | TI
         | TB
         | TU

instance Arbitrary Typ where
  arbitrary = elements [TC, TI, TB, TU]
-}

data TYPE a b = PROD (ExpG (a, b))
              | SUM  (ExpG (Either a b))
              | EXP  (ExpG (a -> b))
              | LIST (ExpG [a])

instance (Arbitrary (ExpG a), Arbitrary (ExpG b)) => Arbitrary (TYPE a b) where
  arbitrary = do n <- choose (0, 3)
                 case (n :: Int) of
                      0 -> PROD <$> arbitrary
                      1 -> SUM  <$> arbitrary
                      2 -> EXP  <$> arbitrary
                      3 -> LIST <$> arbitrary

{-
class Ty a b | a -> b, b -> a

data TyU = TYU

instance Ty TyU (ExpG ())

instance (Ty t1 e1, Ty t2 e2) =>
          Ty (TyP t1 t2) (ExpG (t1, t2))

data TyP a b = TYP a b

instance Ty a (ExpG e) => Arbitrary (ExpG e) where
  arbitrary = undefined
-}


--instance (Add a b ab) => Add (Succ a) b (Succ ab)


{-
-- | Generate an 'ExpG a' and pass it to 'meb'. Doing these steps
-- together prevents us having to explicitly choose the type 'a'
genExp :: Typ -> Gen (Bool -> Name -> ModuleM [ExportSpec])
genExp TU = meb <$> (arbitrary :: Gen (ExpG ()))
genExp TI = meb <$> (arbitrary :: Gen (ExpG Int))
genExp TB = meb <$> (arbitrary :: Gen (ExpG Bool))
-}
{-
genE :: (Typ -> ExpG a) -> (ExpG a -> c) -> Gen c
genE source sink = do t <- arbitrary
                      return (sink (source t))

foo :: Typ -> Gen (ExpG a)
foo TU = arbitrary :: Gen (ExpG ())
foo TI = arbitrary :: Gen (ExpG Int)

--genF :: Typ -> Gen (Name -> Bool -> ModuleM [ExportSpec])
--genF TU = genE
-}

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
    --, return $ useValue "Prelude" (Symbol "[]")  -- Nil
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
    --, applyE2 dot'   <$> arbitrary <*> arbitrary
    ]

mkNames :: [String] -> Generate [Name]
mkNames = mapM newName

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

modConcat :: ModuleM [a] -> ModuleM [a] -> ModuleM [a]
modConcat a b = do a' <- a
                   b' <- b
                   return (a' ++ b')

modJust :: ModuleM a -> ModuleM (Maybe a)
modJust x = do x' <- x
               return (Just x')

-- | Generate definitions for Names and choose whether to export them
mkMaybeExport :: [Name] -> Gen (ModuleM [ExportSpec])
mkMaybeExport []     = return (return [])
mkMaybeExport (n:ns) = do
  body  <- arbitrary :: Gen (TYPE Int Bool)
  let body' = case body of
                   PROD e -> meb e
                   SUM  e -> meb e
                   EXP  e -> meb e
                   LIST e -> meb e
  specs <- mkMaybeExport ns
  keep  <- arbitrary
  return (modConcat (body' keep n) specs)

mme :: [Name] -> Gen ModuleG
mme ns = do specs <- mkMaybeExport ns
            return (modJust specs)

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
    (names, mods)  <- runGenerate . mkNames <$> listOf genName

    -- Generate bodies for then names
    specs <- mme names

    --export <- arbitrary
    --if export then do
    return (mkMain specs)
    --          else do return (mkMain (return Nothing))
