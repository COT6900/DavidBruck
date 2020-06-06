module Main where

import C6P5

{-|
    Tests 'C6P5.msort' to sort a list by merging msort-ed sublists
-}
main :: IO ()
main = print $ show $ msort [9, 1, 8, 2, 7, 3, 6, 4, 5]
