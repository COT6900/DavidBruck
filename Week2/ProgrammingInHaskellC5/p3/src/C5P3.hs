{-|
Module      : C5P3
Description : Programming in Haskell, chapter 5, problem 3
Maintainer  : David Bruck
-}
module C5P3
    ( pyths
    ) where

{-|
    Finds all combinations of natural numbers (x, y, z)
    where the max of (x, y, z) = z
        and z <= the 'max' parameter
        and x^2 + y^2 = z^2 (pythagorean)
-}
pyths max = filter (\(x, y, z) -> x * x + y * y == z * z) allCombinations

    where
    -- 1^2 + 1^2 > 1^2, so z > 1; similarly, x < z && y < z;
    -- also, allows for x == y, but I'm not sure that's possible?
    allCombinations = concatMap
        (\z -> [(x, y, z) | x <- [1..z - 1], y <- [1..z - 1]])
        [2..max] -- z
