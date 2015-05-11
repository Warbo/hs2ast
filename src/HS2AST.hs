module HS2AST where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Map as DM
import           GhcMonad
import           Module
import           Name
import           HS2AST.Parser
import           HS2AST.Sexpr
import           HS2AST.Types

getAsts :: [HsFile] -> Ghc (Sexpr String)
getAsts fs = do bindings <- namedBindingsFrom fs
                return (collateBindings bindings)

renderAsts :: Sexpr String -> String
renderAsts = show
