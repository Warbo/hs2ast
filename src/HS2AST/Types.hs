{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module HS2AST.Types where

import           Control.Monad
import           Data.Aeson
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Text       as AT
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Data
import           Data.Maybe
import           Data.Stringable
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           FastString
import           Module
import           Name
import           Packages

-- | Arbitrary rose trees
data Sexpr a = Leaf a | Node [Sexpr a] deriving (Eq, Typeable, Data)

type AST = L.Lisp

data Identifier = ID {
    idPackage :: T.Text
  , idModule  :: T.Text
  , idName    :: T.Text
  } deriving (Eq, Ord)

instance ToJSON Identifier where
  toJSON i = object [
      "package" .= idPackage i
    , "module"  .= idModule  i
    , "name"    .= idName    i
    ]

pmn x = do p <- x .: "package"
           m <- x .: "module"
           n <- x .: "name"
           return (p, m, n)

instance FromJSON Identifier where
  parseJSON (Object x) = do
    (p, m, n) <- pmn x
    return ID {
        idPackage = p
      , idModule  = m
      , idName    = n
      }
  parseJSON _ = mzero

instance Show Identifier where
  show = toString . encode . toJSON

data Out   = Out {
    outPackage :: T.Text
  , outModule  :: T.Text
  , outName    :: T.Text
  , outAst     :: L.Lisp
  }

instance ToJSON Out where
  toJSON o = object [
      "package" .=           outPackage o
    , "module"  .=           outModule  o
    , "name"    .=           outName    o
    , "ast"     .= L.encode (outAst     o)
    ]

instance FromJSON Out where
  parseJSON (Object x) = do
    (p, m, n) <- pmn x
    rawAst <- x .: "ast"
    ast    <- case AB.maybeResult . AB.parse L.lisp . TE.encodeUtf8 $ rawAst of
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
  show n = let n' = getOccString n
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

instance Show Module where
  show m = "MODULE: " ++ moduleNameString (moduleName m)

instance Show PackageName where
  show (PackageName fs) = unpackFS fs

type PackageDb = PackageKey -> Maybe PackageName
