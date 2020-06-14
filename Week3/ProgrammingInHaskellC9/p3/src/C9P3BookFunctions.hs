{-|
    "Game of life" code adapted from Programming in Haskell, chapter 9,
    Graham Hutton, Cambridge University Press, 2007

    Noted modifications - David Bruck (search "modification" without quotes)
-}
module C9P3BookFunctions
    ( life
    , cls
    , showcells
    , nextgen
    , writeat
    , height -- Used by problem 4
    , wrap -- Used by problem 4
    ) where

-- modification to import threadDelay, hFlush, and stdout (all used for
-- enhanced operation of timings and console output, cross-platform)
-- - David Bruck
import Control.Concurrent
    ( threadDelay
    )
import System.IO
    ( hFlush
    , stdout
    )

-- size of board
width   = 20
height  = 20

-- stdout logic and game logic functions
-- modification to some syntax to make hlint happy only - David Bruck
seqn []         = return () :: IO ()
seqn (a:as)     = do a
                     seqn as
goto (x, y)     = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- |Book implementation of writing a string at a particular position
writeat p xs    = do goto p
                     putStr xs

-- |Book implementation of clearing the game board in the console
cls             = putStr "\ESC[2J"

-- |Book implementation of showing "O" character at input positions
showcells b     = seqn [writeat p "O" | p <- b]

isAlive b p     = p `elem` b
isEmpty b       = not . isAlive b

-- |Book implementation of wrapping around by columns/rows past height/width
wrap (x, y)     = ( ((x - 1) `mod` width) + 1
                  , ((y - 1) `mod` height) + 1
                  )
neighbs (x, y)  = map wrap [ (x - 1, y - 1)
                           , (x    , y - 1)
                           , (x + 1, y - 1)
                           , (x - 1, y    )
                           , (x + 1, y    )
                           , (x - 1, y + 1)
                           , (x    , y + 1)
                           , (x + 1, y + 1)
                           ]
liveneighbs b   = length . filter (isAlive b) . neighbs
survivors b     = [p | p <- b, liveneighbs b p `elem` [2, 3]]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs)
births b        = [p | p <- rmdups $ concatMap neighbs b
                     , isEmpty b p
                     , liveneighbs b p == 3]

{-|
    Book implementation of the main game logic for the "game of life"
    (gets the new board for each generation)
-}
nextgen b       = survivors b ++ births b

-- |Book implementation of the "game of life"
life b          = do cls
                     showcells b

                     -- modification to draw immediately instead of only
                     -- after the console output buffer fills each time
                     -- - David Bruck
                     hFlush stdout

                     -- modification to replace 'wait' - David Bruck
                     threadDelay 40000

                     life $ nextgen b
