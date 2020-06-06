{-|
Module      : C10P1
Description : Programming in Haskell, chapter 10, problem 1
Maintainer  : David Bruck
-}
module C10P1
    ( multiply
    ) where

import C10P1BookFunctions
    ( value
    , Expr (..)
    )

{-|
    Using only 'C10P1BookFunctions.Add', 'multiply' creates an expression
    tree which can be evaluated to the the product of two natural numbers
-}
multiply x y = value $ mult x y where 
    mult 0 y = Val 0
    mult x y = Add (Val y) (mult (x - 1) y)
