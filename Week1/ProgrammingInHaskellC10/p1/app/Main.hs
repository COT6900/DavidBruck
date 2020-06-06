module Main where

import C10P1

import C10P1BookFunctions
    ( value
    , Expr (..)
    )

-- |Tests 'C10P1.multiply' with hardcoded parameters 5 and 6 with expected result: 30
main :: IO ()
main = print $ value $ multiply (Val 5) (Val 6)
