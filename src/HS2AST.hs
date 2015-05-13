module HS2AST where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import qualified Data.Map as DM
import           GhcMonad
import           Module
import           Name
import           HS2AST.Parser
import           HS2AST.Sexpr
import           HS2AST.Types
import           System.Directory

getAsts :: [HsFile] -> Ghc (Sexpr String)
getAsts fs = do bindings <- namedBindingsFrom fs
                return (collateBindings bindings)

identifiedAsts :: [HsFile] -> Ghc (Sexpr String)
identifiedAsts fs = do bindings <- namedBindingsFrom fs
                       return (collateIdentified (identifyAsts bindings))

renderAsts :: Sexpr String -> String
renderAsts = show

mkDir = createDirectoryIfMissing True . intercalate "/"

leafList (Node [])            = []
leafList (Node (Leaf x : xs)) = x : leafList (Node xs)

writeToFile :: Sexpr String -> IO ()
writeToFile (Node [ids, ast]) = let path = leafList ids
                                in  do mkDir (init path)
                                       writeFile (intercalate "/" path)
                                                 (show ast)

writeAsts (Node asts) = mapM writeToFile asts
