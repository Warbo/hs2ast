{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}

module HS2AST.Sexpr where

import qualified Data.AttoLisp               as L
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.Data
import           Data.Generics
import           Data.Generics.Uniplate.Data
import           Data.Maybe
import qualified Data.Stringable             as S
import           HS2AST.Types
import           HsBinds
import           Literal
import           Name
import           SrcLoc
import           TypeRep
import           Var


mkLeaf :: String -> L.Lisp
mkLeaf x = L.String (S.fromString x)

mkNode :: [L.Lisp] -> L.Lisp
mkNode xs = L.List xs



convertBinding :: HsBindLR Name Name -> Maybe L.Lisp
convertBinding = simpleAst

-- | Convert Data instances to s-expressions
toSexp :: Data a => a -> Maybe L.Lisp
toSexp x = let tail = gmapQ toSexp  x
               head = toSx  x
               in  case head of
                    Nothing -> Nothing
                    Just y  -> Just (mkNode (y : catMaybes tail))

toSx :: Data a =>  a -> Maybe L.Lisp
toSx x = let l = mkLeaf (strConstr x)
         in  Just l

simpleAst :: Data a => a -> Maybe L.Lisp
simpleAst = toSexp

strConstr :: Data a => a -> String
strConstr = extQ (extQ (extQ (show . toConstr) showBS) showNameString) showVar

showVar :: Var -> String
showVar = show . Var.varName


showBS :: ByteString -> String
showBS bs = "BS(" ++ unpack bs ++ ")"

showNameString :: Name -> String
showNameString n = "Name(" ++ show n ++ ")"
