module Main where

import C5P3
import Text.Printf
    ( printf
    )
import Data.List
    ( intercalate
    )
import Data.Function
    ( (&)
    )

{-|
    Tests 'C5P3.pyths' to generate all pythagorean combinations
        (x^2 + y^2 = z^2) for z <= max (hardcoded for testing at 10)
-}
main :: IO ()
main = map
           (\(x, y, z) -> printf "(%d, %d, %d)" x y z)
           (pyths (10 :: Int)) {- pyths has polymorphic input,
                                  but '10' is apparently too ambiguous
                               -}
       & intercalate ",\n  "
       & printf "[ (x, y, z), ... ]:\n[ %s ]"
