module Main where

import C10P4

{-|Tests 'C10P4.balance' with some unsorted (actually reverse-sorted) lists
   of varying types, and also tests an empty list (prints "empty")
-}
main :: IO ()
main = do {
    print (balance ([] :: [Int]));                          -- depth 0 (empty)
    print (balance ["Darn", "Cats", "Believe", "Anything"]);-- [String], depth 3
    print (balance [4.321, 3.21, 2.1, 1.0]);                -- [Double], depth 3
    print (balance [9, 8, 7, 6, 5, 4, 3, 2, 1])             -- [Int], depth 4
}
