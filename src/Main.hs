module Main where

import System.Environment
import HS2AST
import HS2AST.Types

-- Get the output directory from our arguments
main = do args <- getArgs
          if null args
             then error "Must provide an output directory"
             else return ()
          let out = head args

          -- Get our input files from stdin
          input <- getContents
          let files = lines input

          -- Extract ASTs
          bindings <- runInSession (identifiedAsts (toHs files))

          -- Write to dirs
          writeAsts out bindings
