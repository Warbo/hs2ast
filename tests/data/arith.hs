module Arith (Nat, plus, mult, two) where

data Nat = Z | S Nat deriving (Show)

plus  Z    y = data
plus (S x) y = S (plus x y)

mult  Z    y = Z
mult (S x) y = plus y (mult x y)

pred  Z    = Z
pred (S x) = x

two = S (S Z)

main = print (mult two two)
