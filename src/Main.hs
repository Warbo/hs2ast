module Main where

import System.Environment
import HS2AST
import HS2AST.Types

          -- Get our input files from stdin
main = do input <- getContents

          -- Build ASTs
          result <- filesToAsts (lines input)

          -- Print results to stdout
          putStr (unlines result)
