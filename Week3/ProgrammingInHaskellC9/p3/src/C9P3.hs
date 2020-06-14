{-|
Module      : C9P3
Description : Programming in Haskell, chapter 9, problem 3
Maintainer  : David Bruck
-}
module C9P3
    ( flickerFreeLife
    ) where

import C9P3BookFunctions
    ( cls
    , showcells
    , nextgen
    , writeat
    , height
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( forM_
    )
import System.IO
    ( hFlush
    , stdout
    )
import Data.List
    ( sort
    )

{-
    One easy way to see the effect of changes made by solving
    problem 3 is to change `clearChar = "x"` and it will draw
    the death trails
-}
clearChar = " "

{-|
    Re-implementation of book function 'life' except it does not clear
    the screen before each new board is drawn, it only clears deleted
    spaces, and only draws to new spaces (and does not redraw spaces
    with no changes)
-}
flickerFreeLife b   = do cls
                         writeat (1, height + 1) "Flicker-free \"Game of life\""
                         writeat (1, height + 2) "Ctrl+C to quit"
                         lifeImpl [] (sort b) where 
    lifeImpl old new = do
        let (deaths, births) = getChanges old new

        forM_ deaths (`writeat` clearChar)

        showcells births

        -- draw immediately instead of only after the console
        -- output buffer fills each time
        hFlush stdout

        threadDelay $ 250 * 1000 -- microseconds; total 0.25 seconds

        lifeImpl new (sort (nextgen new))

    -- requires sorted inputs, advances "cursor" and compares firsts
    getChanges []           new         = ([],    new)
    getChanges old          []          = (old,   [] )
    getChanges old@(x:xs)   new@(y:ys)
        | x < y     = let (deleted, created) = getChanges xs new
                    in (x : deleted, created)
        | x > y     = let (deleted, created) = getChanges old ys
                    in (deleted, y : created)
        | otherwise = getChanges xs ys
