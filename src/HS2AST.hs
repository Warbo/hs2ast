module HS2AST where

import Control.Applicative
import Data.Maybe
import GhcMonad
import HS2AST.Parser
import HS2AST.Sexpr
import HS2AST.Types

getAsts :: [HsFile] -> Ghc [Sexpr String]
getAsts fs = do bindings <- bindingsFrom fs
                return (dumpBindings bindings)
