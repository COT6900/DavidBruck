module C7P7BookFunctions
    ( bookChop8
    , bookMap
    , unfold
    ) where

-- |Book reference function chop8
bookChop8 []    = []
bookChop8 bits  = take 8 bits : bookChop8 (drop 8 bits)

-- |Book reference function map
bookMap f xs    = [f x | x <- xs]

{-|
    The 'unfold' function from the book,
    to be used as a basis of the unfold function versions
-}
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)
