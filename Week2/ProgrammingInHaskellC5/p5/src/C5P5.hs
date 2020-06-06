{-|
Module      : C5P5
Description : Programming in Haskell, chapter 5, problem 5
Maintainer  : David Bruck
-}
module C5P5
    ( combinedGenerators
    ) where

{-|
    Alternate implementation of single comprehension:
    [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
    except as a comprehension between two generators
-}
combinedGenerators = zip

    {- xs strategy: replicate each element
       ys strategy: replicate entire list
    
       zip result expected to produce all combinations:
       (x0 (x0 (x0 (x1 (x1 (x1 (x2 (x2 (x2
        y0),y1),y2),y0),y1),y2),y0),y1),y2)
    -}

    (concatMap (replicate $ length xs) xs)
    (concat (replicate (length xs) ys))

    where
    xs = [1, 2, 3]
    ys = [4, 5, 6]
