module Main where

import System.Environment
import HS2AST
import HS2AST.Types

main = do input <- getContents
          let args = lines input
          bindings <- runInSession (identifiedAsts (toHs args))
          writeAsts bindings
