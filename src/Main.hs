module Main where

import System.Environment
import HS2AST

main = do args <- getArgs
          if null args
             then error "Please provide Haskell filenames as arguments"
             else return ()
          bindings <- runInSession (getAsts (toHs args))
          mapM_ (mapM_ (mapM_ print)) bindings
