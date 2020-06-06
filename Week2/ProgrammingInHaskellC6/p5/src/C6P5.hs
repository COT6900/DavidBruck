{-|
Module      : C6P5
Description : Programming in Haskell, chapter 6, problem 5
Maintainer  : David Bruck
-}
module C6P5
    ( msort
    ) where

import C6P4

{-|
    Implements merge sort by using 'C6P4.merge' (which retains order)
    on two halves which are themselves recursively merge-sorted
-}
msort []    = []
msort [x]   = [x]
msort xs    = merge (msort left) (msort right)
              
              where
              (left, right) = splitAt (length xs `div` 2) xs
