{-# LANGUAGE FlexibleInstances, RankNTypes, DeriveDataTypeable #-}

module HS2AST.Sexpr where

import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           SrcLoc
import           HsBinds
import           Data.Generics
import           Name
import           Data.Data
import           HS2AST.Types
import           Data.Maybe

-- Useful for discarding a load of information from GHC's complex AST types
type AST = Sexpr String

convertBinding :: HsBindLR Name Name -> Maybe AST
convertBinding = simpleAst . dummyTypes

excludedTypes, unwrapTypes :: [TypeRep]
excludedTypes = [
                  typeRep (Proxy :: Proxy SrcSpan)
                ]
unwrapTypes   = [
                  typeRep (Proxy :: Proxy (Located Name))
                ]

-- | Convert Data instances to s-expressions
toSexp :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe (Sexpr String)
toSexp ex un x = let tail = gmapQ (toSexp ex un) x
                     head = toSx ex un x
                 in  case head of
                          Nothing -> Nothing
                          Just y  -> Just (mkNode (y : catMaybes tail))

simpleAst :: Data a => a -> Maybe (Sexpr String)
simpleAst = toSexp excludedTypes unwrapTypes

strConstr :: Data a => a -> String
strConstr = extQ (extQ (show . toConstr) showBS) showNameString

showBS :: ByteString -> String
showBS bs = "BS(" ++ unpack bs ++ ")"

showNameString :: Name -> String
showNameString n = "Name(" ++ show n ++ ")"

toSx :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe (Sexpr String)
toSx ex un x = let t = typeRep [x]
                   n = mkNode []
                   l = mkLeaf (strConstr x)
               in  if t `elem` ex
                      then Nothing
                      else Just (if t `elem` un then n else l)
