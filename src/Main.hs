module Main where

import System.Environment
import HS2AST
import HS2AST.Types

main = do args <- getArgs
          if null args
             then error "Please provide Haskell filenames as arguments"
             else return ()
          bindings <- runInSession (identifiedAsts (toHs args))
          writeAsts bindings
