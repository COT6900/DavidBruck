{-|
Module      : C2P5
Description : Programming in Haskell, chapter 2, problem 5
Maintainer  : David Bruck
-}
module C2P5
    ( initOne
    , initTwo
    ) where

initOneImpl _  []       = []
initOneImpl xs [x]      = xs
initOneImpl xs (x:xxs)  = initOneImpl (xs ++ [x]) xxs

-- |Returns an array which contains all but the last element
initOne     = initOneImpl [] {- initOne also has an implicit parameter
                                since partial application was applied to
                                initOneImpl (currying) -}

-- |Returns an array which contains all but the last element
initTwo arr = reverse (tail (reverse arr))