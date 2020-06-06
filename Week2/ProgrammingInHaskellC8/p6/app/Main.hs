module Main where

import C8P6

-- |Tests 'C8P6.eval' with a hardcoded equation with expected result: (512)/2-2^8=0
main :: IO ()
-- √ == sqrt
-- ↑ == ^ ("to the power of")
main = print $ eval "(2*254 + 4)/sqrt(2↑2) - 2^( √4*4 )"
