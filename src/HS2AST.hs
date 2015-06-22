module HS2AST where

import           Data.List
import           GhcMonad
import           Module
import           Name
import           HS2AST.Parser
import           HS2AST.Sexpr
import           HS2AST.Types

convertToNamed f (a, b, c, d) = case convertBinding d of
                                     Nothing -> []
                                     Just y  -> [((f, a, b, c), y)]

namedAsts :: HsFile -> Ghc [Named OutName AST]
namedAsts f = do bindings <- namedBindingsFrom f
                 return $ concatMap (convertToNamed (unHs f)) bindings

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
