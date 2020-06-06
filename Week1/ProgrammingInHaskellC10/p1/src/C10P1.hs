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
multiply (Val 0) y  = Val 0
multiply x y        = Add y (multiply (Val (value x - 1)) y)
