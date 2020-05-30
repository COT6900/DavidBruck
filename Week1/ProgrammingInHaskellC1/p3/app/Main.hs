module Main where

import qualified C1P3 ( product )

-- |Tests 'C1P3.product' with a hardcoded list [2, 3, 4] with expected result: 24
main :: IO ()
main = print (C1P3.product [2, 3, 4])
