{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module ML4HS.Sexpr where

import Data.Data
import Data.Dynamic
import Data.Typeable
import Data.Generics.Uniplate.Data
import HscTypes
import SrcLoc

-- A simple rose tree for ASTs. We use tags of type 'a' instead of GHC's complex
-- multitude of mutually-recursive types.

data Sexpr a = Sx a [Sexpr a] deriving (Typeable, Data)

instance Show (Sexpr String) where
  show (Sx x xs) = "(" ++ unwords (x : map show xs) ++ ")"

{-
instance (Data a) => Show (Sexpr a) where
  show (Sx x xs) = let h = showConstr (toConstr x)
                       t = map show xs
                   in  "(" ++ unwords (h:t) ++
-}

-- Convert GHC's complicated AST types to Sexpr

toSs :: Data a => a -> Sexpr String
toSs x = Sx (show (toConstr x)) (gmapQ toSs x)

--toSexpr :: (Typeable a) => a -> Sexpr Dynamic
--toSexpr x = Sx (toDyn x) [] -- (map toSexpr (childrenBi x))

{-
toSexpr :: Data a => a -> Sexpr Dynamic
toSexpr x = let m = gmapQ toSexpr x
            in  Sx (toDyn x) m

stripLocations :: Sexpr Dynamic -> Sexpr Dynamic
stripLocations (Sx x xs) = let to   = unLoc
                               from = fromDynamic
                           in case from x of
                                   Nothing -> Sx x (map stripLocations xs)
                                   Just x' -> toSexpr (to x')
-}
--class (Typeable a, Data a) => Sexpressible a where
--  toSexpr2 :: a -> Sexpr Dynamic
--  toSexpr2 x = Sx (toDyn x) . map toSexpr . childrenBi $ x

--instance  => Sexpressible a where
--  toSexpr x = Sx (show x) . map toSexpr . childrenBi $ x

{-
__ = undefined

keepTypes = [
              typeOf (__ :: Pat   Name)
            --, typeOf (__ :: Match Name)
            ]

keepers x = [y | y <- universe x, typeOf y `elem` keepTypes]

locateds x = [y | L _ y <- universe x]

dump :: Uniplate a => a -> String
dump = para (\_ cs -> concat (["("] ++ cs ++ [")"]))

{-
--genDump :: (Uniplate on) => (on -> [String] -> String) -> on -> String
genDump = let
          in  para

--dumpDef (FunBind )
-}

--extractDefs :: HsGroup Name -> HsValBinds Name
-}
