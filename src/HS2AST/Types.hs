{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module HS2AST.Types where

import Data.Aeson
import Data.Data
import Data.Stringable
import Module
import Name
import Packages

-- | Arbitrary rose trees
data Sexpr a = Leaf a | Node [Sexpr a] deriving (Eq, Typeable, Data)

type AST = Sexpr String

-- FIXME: Replace Sexpr with some existing implementation. sexp and sexpr don't
-- build with GHC 7.10, but attolisp does

data Out   = Out {
    outPackage :: String
  , outModule  :: String
  , outName    :: String
  , outAst     :: AST
  }

instance ToJSON Out where
  toJSON o = object [
      "package" .=       outPackage o
    , "module"  .=       outModule  o
    , "name"    .=       outName    o
    , "ast"     .= show (outAst     o)
    ]

instance Show Out where
  show = toString . encode . toJSON

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
