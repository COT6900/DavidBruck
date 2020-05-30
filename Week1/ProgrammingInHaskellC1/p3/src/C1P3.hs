{-|
Module      : C1P3
Description : Programming in Haskell, chapter 1, problem 3
Maintainer  : David Bruck
-}
module C1P3
    ( C1P3.product
    ) where

-- 'product' is also imported by Prelude,
-- so all references need be explicitly prefixed by module

-- |Produces the product of multiplying elements of input list
product []      = 1 {-  Identity "base" state for multiplication is:
                        element * identity == element
                        Therefore, identity == 1 -}
product (x:xs)  = x * C1P3.product xs
