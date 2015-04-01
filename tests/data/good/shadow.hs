module Shadow where

f f = f

g x = let h x = g x
       in x (h x)
