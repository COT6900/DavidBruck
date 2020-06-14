module Main where

import C12

{-|
    Tests 'C12.movesToReachIn' to find all combinations of chess moves for a
    Knight to be able to move from a position to another position in n moves or
    less.
-}
main :: IO ()
main = print $ (6, 2) `movesToReachIn3` (6, 1)

    where
    movesToReachIn3 = movesToReachIn 3