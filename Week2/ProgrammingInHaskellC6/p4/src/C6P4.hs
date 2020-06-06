{-|
Module      : C6P4
Description : Programming in Haskell, chapter 6, problem 4
Maintainer  : David Bruck
-}
module C6P4
    ( merge
    ) where

{-|
    Assuming both list inputs are sorted,
    'merge' concatenates elements to form one sorted list
-}
{- main case first: take each lowest element and
   append to the merge of everything else
-}
merge (x : xs)  (y : ys)    | x < y     = x : merge xs          (y : ys)
                            | otherwise = y : merge (x : xs)    ys
merge xs        ys          = xs ++ ys {- one or both lists are empty;
                                          and the other one's sorted
                                       -}
