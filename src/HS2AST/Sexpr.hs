{-# LANGUAGE FlexibleInstances, RankNTypes, DeriveDataTypeable #-}

module HS2AST.Sexpr where

import qualified Data.AttoLisp   as L
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Data
import Data.Generics
import Data.Generics.Uniplate.Data
import Data.Maybe
import qualified Data.Stringable as S
import HS2AST.Types
import HsBinds
import Name
import SrcLoc
import TypeRep

mkLeaf :: String -> L.Lisp
mkLeaf x = L.String (S.fromString . dummyTypes $ x)

mkNode :: [L.Lisp] -> L.Lisp
mkNode xs = L.List (dummyTypes xs)

-- | Replace all Types in an AST with a dummy, to avoid pre-typecheck errors
dummyTypes :: Data a => a -> a
dummyTypes = transformBi (const (LitTy (NumTyLit 0)))

convertBinding :: HsBindLR Name Name -> Maybe L.Lisp
convertBinding = simpleAst . dummyTypes

excludedTypes, unwrapTypes :: [TypeRep]
excludedTypes = [
                  typeRep (Proxy :: Proxy SrcSpan)
                ]
unwrapTypes   = [
                  typeRep (Proxy :: Proxy (Located Name))
                ]

-- | Convert Data instances to s-expressions
toSexp :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe L.Lisp
toSexp ex un x = let tail = gmapQ (toSexp ex un) x
                     head = toSx ex un x
                 in  case head of
                          Nothing -> Nothing
                          Just y  -> Just (mkNode (y : catMaybes tail))

simpleAst :: Data a => a -> Maybe L.Lisp
simpleAst = toSexp excludedTypes unwrapTypes

strConstr :: Data a => a -> String
strConstr = extQ (extQ (show . toConstr) showBS) showNameString

showBS :: ByteString -> String
showBS bs = "BS(" ++ unpack bs ++ ")"

showNameString :: Name -> String
showNameString n = "Name(" ++ show n ++ ")"

toSx :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe L.Lisp
toSx ex un x = let t = typeRep [x]
                   n = mkNode []
                   l = mkLeaf (strConstr x)
               in  if t `elem` ex
                      then Nothing
                      else Just (if t `elem` un then n else l)
