module Main where

import C9P3

-- |Tests 'C9P3.flickerFreeLife' with the book-provided "glider"
main :: IO ()
-- change 'flickerFreeLife' to 'life' to see old book-behavior
main = flickerFreeLife [ (4, 2)
                       , (2, 3)
                       , (4, 3)
                       , (3, 4)
                       , (4, 4)
                       ] -- Book-provided glider
