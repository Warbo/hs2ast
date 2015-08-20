{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module HS2AST.Types where

import           Control.Monad
import           Data.Aeson
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import           Data.Data
import           Data.Stringable
import           Module
import           Name
import           Packages

-- | Arbitrary rose trees
data Sexpr a = Leaf a | Node [Sexpr a] deriving (Eq, Typeable, Data)

type AST = Sexpr String

-- FIXME: Replace Sexpr with some existing implementation. sexp and sexpr don't
-- build with GHC 7.10, but attolisp does

data Out   = Out {
    outPackage :: String
  , outModule  :: String
  , outName    :: String
  , outAst     :: L.Lisp
  }

instance ToJSON Out where
  toJSON o = object [
      "package" .=                     outPackage o
    , "module"  .=                     outModule  o
    , "name"    .=                     outName    o
    , "ast"     .= toString (L.encode (outAst     o))
    ]

instance FromJSON Out where
  parseJSON (Object x) = do
    p <- x .: "package"
    m <- x .: "module"
    n <- x .: "name"
    rawAst <- x .: "ast"
    ast    <- case AB.maybeResult . AB.parse L.lisp . fromString $ rawAst of
                   Nothing -> mzero
                   Just x  -> return x
    return Out {
        outPackage = p
      , outModule  = m
      , outName    = n
      , outAst     = ast
      }
  parseJSON _ = mzero

instance Show Out where
  show = toString . encode . toJSON

instance Functor Sexpr where
  fmap f (Leaf x)  = Leaf (f x)
  fmap f (Node xs) = Node (map (fmap f) xs)

instance Show a => Show (Sexpr a) where
  show (Leaf x)  = show x
  show (Node xs) = "(" ++ unwords (map show xs) ++ ")"

-- Helpful orphan instances

instance Show Name where
  show n = let n' = occNameString . nameOccName $ n
            in case nameModule_maybe n of
                    Just m  -> concat [
                                 show (modulePackageKey m),
                                 ":",
                                 show (moduleName m),
                                 ".",
                                 n']
                    Nothing -> n'


instance Show ModuleName where
  show = moduleNameString

instance Show PackageKey where
  show = packageKeyString
