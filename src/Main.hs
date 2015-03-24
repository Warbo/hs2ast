{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import Control.Applicative
import Tip
import Tip.HaskellFrontend
import Tip.Params

data Term = FromGhc Id | Fresh Int
  deriving (Show,Eq,Ord)

disambigId :: Id -> [Term]
disambigId i = case i of
                GHCOrigin name _ _ -> getUnique name vs : [ Refresh vs x | x <- [0..] ]
                GHCPrim   po       ->
                Eta       n        ->

  where
    vs = case ppId i of
           "label" -> Label
           []      -> Var "x"
           xs      -> Var xs

main = do thy <- readHaskellFile Params
            { file        = "tests/data/arith.hs"
            , include     = []
            , flags       = []
            , only        = []
            , extra       = []
            , extra_trans = ["plus"]
            }
          return thy
