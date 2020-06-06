{-|
Module      : C7P7
Description : Programming in Haskell, chapter 7, problem 7
Maintainer  : David Bruck
-}
module C7P7
    ( unfoldChop8
    , unfoldMap
    , unfoldIterate
    ) where

import C7P7BookFunctions
    ( unfold
    )

-- |Custom implementation of function chop8 using 'unfold'
unfoldChop8     = unfold null           (take 8)    (drop 8)

-- |Custom implementation of function map using 'unfold'
unfoldMap f     = unfold null           (f . head)  tail

-- |Custom implementation of function iterate using 'unfold'
unfoldIterate   = unfold (const False)  id
