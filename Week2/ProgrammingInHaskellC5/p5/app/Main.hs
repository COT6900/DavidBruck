module Main where

import C5P5
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
    Tests 'C5P5.combinedGenerators' alternate implementation with two
    generators instead of just the book-provided one
-}
main :: IO ()
main = map
           (uncurry $ printf "(%d, %d)")
           combinedGenerators
       & intercalate ",\n  "
       & printf "[ (x, y), ... ]:\n[ %s ]"
