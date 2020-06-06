module Main where

import C6P4

{-|
    Tests 'C6P4.merge' to merge two sorted input lists into
    a sorted output with all elements
-}
main :: IO ()
main = print $ show $ merge [1, 2, 4, 5, 6] [3, 7, 8, 9, 10]
