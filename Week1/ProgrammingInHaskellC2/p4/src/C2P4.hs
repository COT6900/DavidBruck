{-|
Module      : C2P4
Description : Programming in Haskell, chapter 2, problem 4
Maintainer  : David Bruck
-}
module C2P4
    ( C2P4.last
    ) where

-- 'last' is also imported by Prelude,
-- so all references need be explicitly prefixed by module

-- |Returns 'Just' a list's last element, or 'Nothing' for an empty list
last []     = Nothing
last [x]    = Just x
last (x:xs) = C2P4.last xs
