{-|
Module      : C1P4
Description : Programming in Haskell, chapter 1, problem 4
Maintainer  : David Bruck
-}
module C1P4
    ( reverseQSort
    ) where

{-|Returns input list sorted in descending order
   Modified from qsort as provided in book Programming in Haskell, chapter 1
   NOTE: not a true quicksort because it does not randomly choose the
         partition point; instead, it always uses the first element on
         each partition which means it would have running time of O(n^2)
         on already-sorted inputs
-}
reverseQSort []     = []
reverseQSort (x:xs) = reverseQSort larger ++ [x] ++ reverseQSort smaller
                      where smaller = [a | a <- xs, a <= x]
                            larger  = [b | b <- xs, b > x]
