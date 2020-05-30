module Main where

import C1P4

{-|Tests 'C1P4.reverseQSort' with a hardcoded list of unsorted range (1..9)
   with expected output being list sorted in descending order (opposite qsort)
-}  
main :: IO ()
main = print (reverseQSort [1, 9, 2, 8, 3, 7, 4, 6, 5])
