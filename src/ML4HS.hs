module ML4HS where

import Control.Applicative
import Data.Maybe
import GhcMonad
import ML4HS.Parser
import ML4HS.Sexpr
import ML4HS.Types

getAsts :: [HsFile] -> Ghc [Sexpr String]
getAsts fs = do bindings <- bindingsFrom fs
                return (dumpBindings bindings)
