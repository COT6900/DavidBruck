module Main where

import C10P1

import C10P1BookFunctions
    ( nat2int
    , int2nat
    , Nat (..)
    )

-- |Tests 'C10P1.multiply' with hardcoded parameters 5 and 6 with expected result: 30
main :: IO ()
main = print $ nat2int $ multiply (int2nat 5) (int2nat 6)
