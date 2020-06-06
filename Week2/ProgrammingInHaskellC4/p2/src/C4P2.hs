{-|
Module      : C10P4
Description : Programming in Haskell, chapter 4, problem 2
Maintainer  : David Bruck
-}
module C4P2
    ( safetail
    ) where

-- (a) conditional expression
--safetail xs = if null xs
--              then []
--              else tail xs

-- (b) guarded equations
--safetail xs | null xs   = []
--            | otherwise = tail xs

-- (c) pattern matching

-- |Like 'tail' except returns an empty list for an empty input list
safetail []         = []
safetail (_ : xs)   = xs

-- (alternative pattern matching)
--safetail xs = case xs of
--                  (_ : xxs) -> xxs
--                  _         -> []
--
-- If you build with compiler option -XLambdaCase:
--safetail = \case
--               (_ : xs)  -> xs
--               _         -> []