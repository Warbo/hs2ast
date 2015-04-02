module Main where

import System.Environment
import ML4HS.Parser
import ML4HS.Types

main = do args <- getArgs
          runInSession (bindingsFrom (toHs args))
