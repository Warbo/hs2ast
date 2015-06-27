{-# LANGUAGE DeriveDataTypeable #-}

module HS2AST.Types (
   Sexpr(Leaf, Node)
 , mkLeaf
 , mkNode
 , dummyTypes
 ) where

import Control.Exception (SomeException, try)
import Data.Char
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Maybe
import Module
import Name
import Packages
import System.Directory
import System.Exit
import System.Process
import TypeRep

-- | Arbitrary rose trees
data Sexpr a = Leaf a | Node [Sexpr a] deriving (Eq, Typeable, Data)

mkLeaf :: Data a => a -> Sexpr a
mkLeaf x = Leaf (dummyTypes x)

mkNode :: Data a => [Sexpr a] -> Sexpr a
mkNode xs = Node (dummyTypes xs)

-- | Replace all Types in an AST with a dummy, to avoid pre-typecheck errors
dummyTypes :: Data a => a -> a
dummyTypes = transformBi (const (LitTy (NumTyLit 0)))

instance Functor Sexpr where
  fmap f (Leaf x)  = Leaf (f x)
  fmap f (Node xs) = Node (map (fmap f) xs)

instance Show a => Show (Sexpr a) where
  show (Leaf x)  = show x
  show (Node xs) = "(" ++ unwords (map show xs) ++ ")"

-- Helpful instances
instance Show Name where
  show = occNameString . nameOccName

instance Show ModuleName where
  show = moduleNameString

instance Show PackageKey where
  show = packageKeyString
