{-|
Module      : C10P1
Description : Programming in Haskell, chapter 10, problem 1
Maintainer  : David Bruck
-}
module C10P1
    ( multiply
    ) where

{-|
    Using only mathematical (+) addition and (-) subtraction operators,
    finds the product of two natural numbers
-}
multiply x y | x == 0       = 0
             | x == 1       = y
             | otherwise    = y + multiply (x - 1) y
