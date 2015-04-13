{-# LANGUAGE FlexibleInstances, RankNTypes, DeriveDataTypeable #-}

module ML4HS.Sexpr where

import HsBinds
import Data.Generics
import Name
import Data.Data
import Data.Functor.Identity
import ML4HS.Types
import Data.Maybe
import Control.Monad

-- Useful for discarding a load of information from GHC's complex AST types

dumpBinding :: HsBindLR Name Name -> Maybe (Sexpr String)
dumpBinding = simpleAst . dummyTypes

excludedTypes, unwrapTypes :: [TypeRep]
excludedTypes = [
                ]
unwrapTypes   = [
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

toSx :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe (Sexpr String)
toSx ex un x = let s = show (toConstr x)
                   t = typeRep [x]
               in  if t `elem` ex
                      then Nothing
                      else Just (if t `elem` un
                                    then mkNode []
                                    else mkLeaf s)
