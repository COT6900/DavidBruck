{-|
Module      : C11P4
Description : Programming in Haskell, chapter 11, problems 4 and 5
Maintainer  : David Bruck
-}
module C11P4
    ( countExpressionCombinations
    , countValidCombinations
    ) where

import C11P4BookFunctions
    ( Expr
    , exprs
    , choices
    , eval
    )

expressionCombinations numbers = map exprs $ choices numbers

-- |Counts all possible combinations of expressions for a set of input numbers
countExpressionCombinations numbers =
    sum $ map length $ expressionCombinations numbers

-- |Counts all valid expressions that can be formed for a set of input numbers
countValidCombinations generalized numbers =
    sum $ map (length . validExpressions) $ expressionCombinations numbers

    where
    {-
        generalized of 'False' is for problem 4 limiting on positive Int;
        while 'True' is for problem 5 generalizing to include all integrals
        (including zero and negative)
    -}
    validExpressions = filter (not . null . eval generalized)
