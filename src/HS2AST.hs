module HS2AST where

import Data.List
import HS2AST.Parser
import HS2AST.Sexpr
import HS2AST.Types
import GHC
import GhcMonad
import Module
import Name

convertToNamed :: Named a (HsBindLR Name Name) -> [Named a AST]
convertToNamed (a, b) = case convertBinding b of
                             Nothing -> []
                             Just b' -> [(a, b')]

namedAsts :: HsFile -> Ghc [Named OutName AST]
namedAsts f = do bindings <- namedBindingsFrom f
                 return $ concatMap convertToNamed bindings

type Dir  = FilePath
type File = FilePath

filesToAsts' :: [HsFile] -> Ghc [Named OutName AST]
filesToAsts' []     = return []
filesToAsts' (f:fs) = do ast  <- namedAsts    f
                         asts <- filesToAsts' fs
                         return (ast ++ asts)

sanitise :: [FilePath] -> [FilePath]
sanitise = filter (not . dodgy)
  where dodgy x = '\n' `elem` x

filesToAsts :: [FilePath] -> IO [String]
filesToAsts fs = do asts <- runInSession (filesToAsts' (toHs (sanitise fs)))
                    return (map showAst asts)

showAst ((a, b, c, d), ast) = name ++ " " ++ show ast
  where bits1 = [a, show b, show c, show d]
        bits2 = map escape bits1
        bits3 = map quote  bits2
        escape [] = []
        escape  ('"':s) = "\\\"" ++ escape s
        escape ('\\':s) = "\\\\" ++ escape s
        escape    (c:s) = c : escape s
        quote s = "\"" ++ s ++ "\""
        name = "(" ++ intercalate " " bits3 ++ ")"
