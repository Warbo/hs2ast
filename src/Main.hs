{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import System.Environment
import ML4HS.Parser

main = getArgs >>= graphFiles
