{-# LANGUAGE FlexibleInstances, RankNTypes, DeriveDataTypeable #-}

module ML4HS.Sexpr where

import Data.List
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import CoreSyn
import Unique
import Data.Generics.Schemes
import Data.Generics.Uniplate.Operations
import SrcLoc
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

dumpBindings :: [[[HsBindLR Name Name]]] -> [Sexpr String]
dumpBindings              []  = []
dumpBindings (         []:ys) = dumpBindings ys
dumpBindings (    ([]:xs):ys) = dumpBindings (xs:ys)
dumpBindings (((z:zs):xs):ys) = case dumpBinding z of
                                     Nothing -> dumpBindings ((zs:xs):ys)
                                     Just s  -> s : dumpBindings ((zs:xs):ys)

namedToSexpr :: (Data a, Uniquable a) => Env -> (a, Expr a) -> (Env, (String, Sexpr String))
namedToSexpr env (n, e) = let (env', n') = showName env n
                          in  (env, (n', exprToSexpr (fiddleExpr e)))

type Env = [Unique]

lookUp :: Env -> Unique -> (Env, Int)
lookUp env u = case elemIndex u env of
                    Nothing -> (env ++ [u], length env)
                    Just i  -> (env,        i)

showName :: Uniquable a => Env -> a -> (Env, String)
showName env x = let (env', i) = lookUp env (getUnique x)
                 in  (env', show i)

coreToSexpr :: (Uniquable a, Data a) => Bind a -> [(String, Sexpr String)]
coreToSexpr c = case c of
                     NonRec n e -> let (_, r) = namedToSexpr [] (n, e) in [r]
                     Rec    es  -> passEnv [] namedToSexpr es

passEnv :: Env -> (Env -> a -> (Env, b)) -> [a] -> [b]
passEnv env f []     = []
passEnv env f (x:xs) = let (env', y) = f env x
                       in  y : passEnv env' f xs

fiddleExpr :: Data a => Expr a -> Expr a
fiddleExpr = let f (Tick _ x) = x
                 f x          = x
             in  transform f

exprToSexpr :: Data a => (Expr a) -> Sexpr String
exprToSexpr e = case e of
                     Tick _ x -> exprToSexpr x
                     _        -> fromJust $ toSexp [] [] e

{-
                 Var Id
Lit Literal
App (Expr b) (Arg b)
Lam b (Expr b)
Let (Bind b) (Expr b)
Case (Expr b) b Type [Alt b]
Cast (Expr b) Coercion
Tick (Tickish Id) (Expr b)
Type Type
Coercion Coercion

toSexpr :: Data a => a -> [Sexpr String] -> Maybe (Sexpr String)
toSexpr x xs = children

keep :: Data a => a -> Bool
keep x = undefined

unwrap :: Data a => a -> Bool
unwrap x = undefined
-}

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
strConstr = extQ (show . toConstr) showBS

showBS :: ByteString -> String
showBS bs = "BS(" ++ unpack bs ++ ")"

toSx :: Data a => [TypeRep] -> [TypeRep] -> a -> Maybe (Sexpr String)
toSx ex un x = let t = typeRep [x]
                   n = mkNode []
                   l = mkLeaf (strConstr x)
               in  if t `elem` ex
                      then Nothing
                      else Just (if t `elem` un then n else l)

-- | Fold an Sexpr
foldsx :: (a -> b) -> ([b] -> b) -> Sexpr a -> b
foldsx leaf node sx = case unExpr sx of
  Left  x  -> leaf x
  Right xs -> node (map (foldsx leaf node) xs)

anysx :: (a -> Bool) -> Sexpr a -> Bool
anysx f = foldsx f (any id)
