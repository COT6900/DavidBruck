{-|
Module      : C12
Description : Learn You a Haskell, chapter 12, a Knight's quest
Maintainer  : David Bruck
-}
module C12
    ( movesToReachIn
    ) where

{-
    The starting code implementing 'moveKnight', 'in3', and 'canReachIn3'
    provided from Learn You a Haskell website, chapter 12, A Fistful of Monads,
    "A knight's quest": http://learnyouahaskell.com/a-fistful-of-monads

    -- modifications - David Bruck (search "modification" without quotes)
-}

import Control.Conditional
    ( guard
    )
import Data.List
    ( partition
    )
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- Modification: entirely new function 'movesToReachIn' -David Bruck
-- | Get move combinations to reach position within `n` moves
movesToReachIn 0 _      _   = []
movesToReachIn n start  end =
    map (: []) reached ++
        concat (filter
            (not . null)
            (map (movesToReachNext n end) current))
    
    where
    current                         = moveKnight start
    (reached, continue)             = partition (==end) current
    movesToReachNext n end current  = case movesToReachIn (n - 1) current end of
                                          [] -> []
                                          xs -> map (current:) xs
