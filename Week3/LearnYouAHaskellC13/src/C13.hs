{-|
Module      : C13
Description : Learn You a Haskell, chapter 13, Error version of Walk the line
Maintainer  : David Bruck
-}
module C13
    ( landLeft
    , landRight
    , Pole
    ) where

import Control.Monad.Except
    ( ExceptT(..)
    )
import Control.Error.Safe
    ( tryRight
    )

{-
    'Birds' and 'Pol' types were provided from Learn You a Haskell website,
    chapter 12, A Fistful of Monads, "Walk the line":
    http://learnyouahaskell.com/a-fistful-of-monads

    Everything else, I rewrote to support Error monad - David Bruck
-}
type Birds  = Int
type Pole   = (Birds, Birds)

-- |Add n birds to the left, possibly unbalancing tightrope walker
landLeft :: Birds -> Pole -> ExceptT Pole IO Pole
landLeft n  (left,      right       ) =
    land    (left + n,  right       )

-- |Add n birds to the right, possibly unbalancing tightrope walker
landRight :: Birds -> Pole -> ExceptT Pole IO Pole
landRight n (left,      right       ) =
    land    (left,      right + n   )

{-
    Non-exported 'land' used by exported 'landLeft' and 'landRight';
    returns an error with 'tryRight' on unbalanced
-}
land :: Pole -> ExceptT Pole IO Pole
land newPole@(left, right)
    | left - right > 3 || right - left > 3  = tryRight $ Left newPole
    | otherwise                             = return newPole
