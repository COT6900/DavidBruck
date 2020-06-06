{-|
Module      : C10P1
Description : Programming in Haskell, chapter 10, problem 1
Maintainer  : David Bruck
-}
module C10P1
    ( multiply
    ) where

import C10P1BookFunctions
    ( add
    , Nat (..)
    )

{-|
    Using only 'C10P1BookFunctions.add', 'multiply' creates an expression
    tree which can be evaluated to the the product of two natural numbers
-}
multiply x Zero     = Zero
multiply x (Succ y) = add x $ multiply x y
