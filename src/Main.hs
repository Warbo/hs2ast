module Main where

import System.Environment
import ML4HS
--import ML4HS.Parser
--import ML4HS.Types
--import ML4HS.Sexpr

main = do args <- getArgs
          if null args
             then error "Please provide Haskell filenames as arguments"
             else return ()
          bindings <- runInSession (getAsts (toHs args))
          mapM_ (mapM_ (mapM_ print)) bindings
