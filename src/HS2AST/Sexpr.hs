{-# LANGUAGE FlexibleInstances, RankNTypes, DeriveDataTypeable #-}

module HS2AST.Sexpr where

import           Data.List
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import qualified Data.Map as DM
import           CoreSyn
import           Unique
import           Data.Generics.Schemes
import           Data.Generics.Uniplate.Operations
import           SrcLoc
import           HsBinds
import           Data.Generics
import           Name
import           Module
import           Data.Data
import           Data.Functor.Identity
import           HS2AST.Types
import           Data.Maybe
import           Control.Monad

-- Useful for discarding a load of information from GHC's complex AST types

type AST = Sexpr String

convertBinding :: HsBindLR Name Name -> Maybe AST
convertBinding = simpleAst . dummyTypes

collateBindings' ::    [(PackageId, ModuleName, Name, HsBindLR Name Name)]
                    -> DM.Map PackageId (DM.Map ModuleName (DM.Map Name AST))
collateBindings' []                   = DM.empty
collateBindings' ((pid, mn, n, b):xs) = case convertBinding b of
  Nothing  -> collateBindings' xs
  Just ast -> DM.insertWith (DM.unionWith DM.union)
                            pid
                            (DM.singleton mn (DM.singleton n ast))
                            (collateBindings' xs)

collateBindings = pkgsToSexprs . collateBindings'

namesToSexprs :: DM.Map Name AST -> Sexpr String
namesToSexprs = DM.foldrWithKey helper (Node [])
  where helper name ast (Node xs) = Node (Node [Leaf (show name), ast] : xs)

modsToSexprs :: DM.Map ModuleName (DM.Map Name AST) -> Sexpr String
modsToSexprs = DM.foldrWithKey helper (Node [])
  where helper mod names (Node xs) = Node (Node [Leaf (show mod), namesToSexprs names] : xs)

pkgsToSexprs :: DM.Map PackageId (DM.Map ModuleName (DM.Map Name AST)) -> Sexpr String
pkgsToSexprs = DM.foldrWithKey helper (Node [])
  where helper pkg mods (Node xs) = Node (Node [Leaf (show pkg), modsToSexprs mods] : xs)

identifyAsts :: [(PackageId, ModuleName, Name, HsBindLR Name Name)] -> [(Sexpr String, AST)]
identifyAsts = mapMaybe convert
  where convert (pid, mod, name, expr) = case convertBinding expr of
                                             Just ast -> Just (Node [Leaf (show pid),
                                                                     Leaf (show mod),
                                                                     Leaf (show name)],
                                                               ast)
                                             Nothing  -> Nothing

collateIdentified :: [(Sexpr String, AST)] -> Sexpr String
collateIdentified [] = Node []
collateIdentified ((id, ast):xs) = let head      = Node [id, ast]
                                       Node tail = collateIdentified xs
                                   in  Node (head : tail)

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

-- | Fold an Sexpr
foldsx :: (a -> b) -> ([b] -> b) -> Sexpr a -> b
foldsx leaf node sx = case unExpr sx of
  Left  x  -> leaf x
  Right xs -> node (map (foldsx leaf node) xs)

anysx :: (a -> Bool) -> Sexpr a -> Bool
anysx f = foldsx f (any id)
