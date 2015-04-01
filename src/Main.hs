{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import System.Environment
import ML4HS.Parser
import ML4HS.Types

main = do args <- getArgs
          runInSession (parseFiles (toHs args))
