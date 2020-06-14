### David Bruck COT6900 - Week 3

### Programming in Haskell, chapter 9

#### Problem 3

Functions we are copying from the book with some noted modifications, C9P3BookFunctions.hs :

```haskell
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
```



C9P3.hs :

```haskell
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
```



Main.hs :

```haskell
module Main where

import C9P3

-- |Tests 'C9P3.flickerFreeLife' with the book-provided "glider"
main :: IO ()
-- change 'flickerFreeLife' to 'life' to see old book-behavior
main = flickerFreeLife [ (4, 2)
                       , (2, 3)
                       , (4, 3)
                       , (3, 4)
                       , (4, 4)
                       ] -- Book-provided glider
```



#### Problem 4

The book references an internal Hugs function `getCh` which works like `getChar` but does not echo the characters typed into the console back out; however, it is not available for use in a library intended for GHC compilation. While we can disable buffering and prevent echo by setting console options on `stdin`, there are keypresses which cannot be read via `getChar` such as the arrow keys on Windows. Instead, I turned to a cross-platform solution for getting characters by Richard Cook, namely a Haskell package "hidden-char" from https://hackage.haskell.org/package/hidden-char. However, the Haskell package has not been provided for newer GHC versions so I am including the relevant source with my application. Here is Richard Cook's HiddenChar.hs:

```haskell
{-|
Module      : System.IO.HiddenChar
Description : Umbrella module for @System.IO.HiddenChar@
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides a @getHiddenChar@ function which works reasonably
consistently across the Windows, Linux and macOS platforms. @getHiddenChar@
yields a single character from the standard input device with buffering and
echoing to standard output disabled.
-}

{-# LANGUAGE CPP #-}

module System.IO.HiddenChar (getHiddenChar) where

#if defined(OS_LINUX) || defined(OS_MACOS)
import           System.IO.HiddenChar.Posix
#elif defined(OS_WINDOWS)
import           System.IO.HiddenChar.Windows
#else
#error Unsupported platform
#endif
```



Richard Cook also wrote this first of two additional dependencies, Posix.h (for non-Windows consoles) :

```haskell
{-|
Module      : System.IO.HiddenChar.Posix
Description : Posix implementation of @getHiddenChar@
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

Provides the Posix implementation of @getHiddenChar@
-}

module System.IO.HiddenChar.Posix (getHiddenChar) where

import           Control.Exception
import           System.IO

data HandleState = HandleState BufferMode Bool

hGetState :: Handle -> IO HandleState
hGetState h = do
    bufferMode <- hGetBuffering h
    isEcho <- hGetEcho h
    return $ HandleState bufferMode isEcho

hSetState :: Handle -> HandleState -> IO ()
hSetState h (HandleState mode isEcho) = do
    hSetEcho h isEcho
    hSetBuffering h mode

bracketHandle :: Handle -> (IO a -> IO a)
bracketHandle h action = bracket
    (hGetState h)
    (hSetState h)
    (const action)

-- | Read a character from the standard input device with buffering and echoing disabled
getHiddenChar ::
    IO Char -- ^ returned character
getHiddenChar = bracketHandle stdin $ do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    getChar
```



And the second dependency by Richard Cook, Windows.hs (for Windows consoles) :

```haskell
{-|
Module      : System.IO.HiddenChar.Windows
Description : Windows implementation of @getHiddenChar@
Copyright   : (C) Richard Cook, 2017
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

Provides the Windows implementation of @getHiddenChar@
-}

{-# LANGUAGE ForeignFunctionInterface #-}

module System.IO.HiddenChar.Windows (getHiddenChar) where

import           Data.Char
import           Foreign.C.Types

-- Must import as "safe" in order to prevent FFI call from blocking other threads
-- https://github.com/rcook/hidden-char/issues/1
foreign import ccall safe "conio.h _getch" c_getch :: IO CInt

-- | Read a character from the standard input device with buffering and echoing disabled
-- Hack based on http://stackoverflow.com/questions/2983974/haskell-read-input-character-from-console-immediately-not-after-newline
getHiddenChar ::
    IO Char -- ^ returned character
getHiddenChar = fmap (chr . fromEnum) c_getch
```



C9P4.hs (finally my contributions, and the "Game of Life" console-based editor) :

```haskell
{-# LANGUAGE LambdaCase #-}
module C9P4
    ( lifeEditor
    ) where

import C9P3BookFunctions
    ( cls
    , writeat
    , height
    , wrap
    )
import C9P3
    ( flickerFreeLife
    )
import System.IO
    ( hFlush
    , stdout
    )
import System.IO.HiddenChar
    ( getHiddenChar
    )
import Control.Monad.State
    ( StateT
    , runStateT
    , gets
    , modify
    , liftIO
    )

cursorOverEmpty     = "°"
cursorOverFilled    = "Θ"
clearChar           = " "
filledChar          = "O"

data EditorData = EditorData {
    cursor          :: (Integer, Integer),
    alreadyFilled   :: Bool,
    board           :: [(Integer, Integer)]
}

startingState = EditorData (1, 1) False []
lifeEditor = instructions >> do runStateT editorImpl startingState
                                return () where
    instructions = do
        cls
        writeat (1, height + 1) "\"Game of life\" editor by David Bruck"
        writeat (1, height + 2) $ "Arrows keys to move, space to toggle, " ++
                                  "enter to run"
    
    editorImpl :: StateT EditorData IO ()
    editorImpl = do
        alreadyFilled   <- gets alreadyFilled
        cursor          <- gets cursor

        liftIO $ writeat
                     cursor
                     (if alreadyFilled
                      then cursorOverFilled
                      else cursorOverEmpty)
        
        -- draw immediately instead of only after the console
        -- output buffer fills each time
        liftIO $ hFlush stdout

        {-
            Cross-platform wait for character via Richard Cook's
            'System.IO.HiddenChar.getHiddenChar'. Supports control keys.
            Unfortunately, arrow keys are 2-3 of these "hidden" characters.
        -}
        liftIO getHiddenChar >>= processKey

    processKey :: Char -> StateT EditorData IO ()
    processKey key
        {-
            Windows arrow keys are like "\0H"
            except Windows MSYS is "\224H";
            In 'nix it's "\ESC[A"
        -}
        | key
              `elem`
              ['\0', '\224', '\ESC']
                                = do -- ctrl character has 2-3 parts
                                     key2 <- liftIO getHiddenChar
                                     processArrow key2

        -- "Enter key" starts the game
        | key
              `elem`
              ['\r', '\n'  ]    = do board <- gets board
                                     liftIO $ flickerFreeLife board

        -- Ctrl+C to quit
        | key == '\ETX'         = liftIO $ return ()

        -- Space key changes whether position starts alive or dead
        | key == ' '            = do alreadyFilled  <- gets alreadyFilled
                                     board          <- gets board
                                     cursor         <- gets cursor

                                     modify $ \vs ->
                                         vs {alreadyFilled = not alreadyFilled,
                                             board =
                                             if alreadyFilled
                                             then (/= cursor) `filter` board
                                             else cursor : board}
                                     editorImpl -- loop

        -- Unknown key
        | otherwise             = editorImpl -- loop

    -- Arrow keys move the cursor position
    processArrow :: Char -> StateT EditorData IO ()
    processArrow key2 = do alreadyFilled                <- gets alreadyFilled
                           cursor@(cursorX, cursorY)    <- gets cursor

                           liftIO $ if not alreadyFilled
                                    then writeat cursor clearChar
                                    else pure ()
                           arrow <- liftIO $ getArrowKey key2
                           let newCursor = case arrow of
                                           '↑' -> wrap (cursorX, cursorY - 1)
                                           '↓' -> wrap (cursorX, cursorY + 1)
                                           '→' -> wrap (cursorX + 1, cursorY)
                                           '←' -> wrap (cursorX - 1, cursorY)
                                           _   -> cursor

                           if cursor /= newCursor
                           then do board <- gets board
                                   modify $ \vs ->
                                       vs {cursor = newCursor,
                                           alreadyFilled =
                                               newCursor `elem` board}
                           else liftIO $ writeat (1, height + 1) (show key2)
                                     
                           editorImpl -- loop

    getArrowKey :: Char -> IO Char
    getArrowKey = \case
        -- First are the Windows / MSYS keys:
        'H' -> return '↑'
        'P' -> return '↓'
        'M' -> return '→'
        'K' -> return '←'

        {-
            'Nix has a common 2nd character for control characters,
            so we keep reading for the last character...
        -}
        '[' -> getArrowKeyLinux <$> getHiddenChar
        _   -> return '\0'

    getArrowKeyLinux = \case
        'A' -> '↑'
        'B' -> '↓'
        'C' -> '→'
        'D' -> '←'
        _   -> '\0'
```



Main.hs :

```haskell
module Main where

import C9P4

-- |Tests 'C9P4.lifeEditor'
main :: IO ()
main = lifeEditor
```



#### Problem 6

C9P6.hs :

```haskell
{-|
Module      : C9P6
Description : Programming in Haskell, chapter 9, problem 6
Maintainer  : David Bruck
-}
module C9P6
    ( nim
    ) where

import Control.Monad
    ( forM_
    , mzero
    )
import Control.Monad.State
    ( StateT
    , runStateT
    , gets
    , modify
    , liftIO
    )
import Text.Read
    ( readMaybe
    )
import Control.Monad.Trans.Maybe
    ( MaybeT(..)
    , runMaybeT
    )
import System.IO
    ( hFlush
    , stdout
    )
import Control.Exception
    ( evaluate
    )

data NimData    = NimData {
    matchsticks         :: [Int],
    secondPlayersTurn   :: Bool
}

data TurnData   = TurnData {
    row                 :: Int,
    count               :: Int
}

{-|
    Game of Nim from unknown origin;
    Haskell version by David Bruck
-}
nim = do cls
         runStateT nimImpl initialState
         pure ()

      where
      nimImpl :: StateT NimData IO ()
      nimImpl = do matchsticks          <- gets matchsticks
                   secondPlayersTurn    <- gets secondPlayersTurn
                   
                   let oneIndexedMatchsticks = [1..] `zip` matchsticks

                   liftIO $ forM_ oneIndexedMatchsticks
                       (\(i, n) -> putStrLn $ "Row " ++ show i ++ ": "
                           ++ if n == 0
                                  then "--empty--"
                                  else unwords $ replicate n "*")
                   (TurnData row count) <- getTurnData

                   -- Clear board before next turn or victory message
                   liftIO cls

                   let (x, y:ys) = splitAt (row - 1) matchsticks
                   let gameOver = all (\(r, c) -> c == 0 ||
                                          (r == row && c - count == 0))
                                      oneIndexedMatchsticks
                   if gameOver
                   then liftIO $ putStrLn $ victoryMessage secondPlayersTurn
                   else do modify $ \vs ->
                               vs {matchsticks = x ++ (y - count) : ys,
                                   secondPlayersTurn = not secondPlayersTurn}
                           nimImpl

                where
                victoryMessage secondPlayersTurn =
                    (if secondPlayersTurn
                        then "First"
                        else "Second")
                        ++ " player wins!"

      getTurnData :: StateT NimData IO TurnData
      getTurnData =
          do matchsticks        <- gets matchsticks
             secondPlayersTurn  <- gets secondPlayersTurn

             row    <- liftIO $ queryIntInRangeLoop
                           (rowPrompt secondPlayersTurn) $
                           nonEmptyRows matchsticks 1
             count  <- liftIO $ queryIntInRangeLoop countPrompt
                           [1..matchsticks !! (row - 1)]
             return $ TurnData row count

          where
          nonEmptyRows []       _ = []
          nonEmptyRows (x:xs)   i = (if x == 0 then id else (i:)) $
                                        nonEmptyRows xs $ i + 1

          queryIntInRangeLoop prompt range =
              do parsedOrError <- runMaybeT $ queryIntInRange prompt range
                 case parsedOrError of
                     Just parsed    -> return parsed
                     _              -> queryIntInRangeLoop
                                           ("Value must be in range: " ++
                                               show range)
                                           range

          queryIntInRange :: String -> [Int] -> MaybeT IO Int
          queryIntInRange prompt range =
              do liftIO $ putStrLn prompt
                 value  <- liftIO getLine
                 parsed <- MaybeT $ return $ readMaybe value
                 if parsed `elem` range
                     then return parsed
                     else mzero

          rowPrompt secondPlayersTurn =
              (if secondPlayersTurn
                  then "Second"
                  else "First")
                  ++ " player, type the row number to take matchsticks\n"
                  ++ "(must not be an --empty-- row)"
          countPrompt = "Type how many matchsticks to take"

      cls = do putStr "\ESC[2J"
               putStrLn "Game of Nim from unknown origin"
               putStrLn "Haskell version by David Bruck\n"

      initialState = NimData [5, 4, 3, 2, 1] False
```



### Programming in Haskell, chapter 10

#### Problem 6

**Note:** requires library in Parsing.hs from Week 2, Programming in Haskell, chapter 8, problem 6.
Functions we are copying from the book with some noted modifications, C10P6BookFunctions.hs :

```haskell
{-|
    Boolean logic tautology checker code adapted from Programming in Haskell,
    chapter 10, Graham Hutton, Cambridge University Press, 2007
-}
module C10P6BookFunctions
    ( isTaut
    , Prop(..)
    ) where

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

type Assoc k v  = [(k, v)]
type Subst      = Assoc Char Bool

find k t = head [v | (k', v) <- t, k == k']

eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

vars (Const _)      = []
vars (Var x)       = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q

---- Definition 1 of 'bools'
--bools n = map (map conv . make n . int2bin) [0..limit]
--
--    where
--    int2bin 0 = []
--    int2bin n = n `mod` 2 : int2bin (n `div` 2)
--
--    limit = 2 ^ n - 1
--
--    make n bits = take n (bits ++ repeat 0)
--
--    conv 0 = False
--    conv 1 = True

-- Definition 2 of 'bools'
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools $ n - 1

substs p = map (zip vs) $ bools $ length vs
    where vs = rmdups $ vars p

rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/=x) xs)

isTaut p = and [eval s p | s <- substs p]
```



C10P6.hs :

```haskell
{-|
Module      : C10P6
Description : Programming in Haskell, chapter 10, problem 6
Maintainer  : David Bruck
-}
module C10P6
    ( tautChecker
    ) where
import C10P6BookFunctions
    ( isTaut
    , Prop(..)
    )
import Parsing
    ( Parser(..)
    , parse
    , symbol
    , token
    , upper
    , (+++)
    )
import Prelude hiding
    ( Const
    )
import System.IO
    ( hFlush
    , stdout
    )

{-|
    Commandline interface accepting user input expressions as propositions for
    testing for tautology (evaluating if a boolean expression is always true
    for all combinations of parameters)
-}
tautChecker =
    do putStrLn "Tautology checker Commandline interface by David Bruck"
       putStrLn "Parses proposition then evaluates tautology via book method\n"
       putStrLn "Enter proposition using operators:"
       putStrLn "    \">\" : (Implies)"
       putStrLn "    \"!\" : (Not)"
       putStrLn "    \"&\" : (And)"
       putStrLn "    \"True\"/\"False\": (constant value)"
       putStrLn "    Characters A through Z : named boolean variables"
       putStrLn "    Parentheses around sub-expressions optional"
       putStrLn "Example: \"A & A\" prints \"False\""
       putStrLn "Example: \"(A > A) & (B > B)\" prints \"True\""
       putStrLn "Example: \"(A & !(!A)) > A\" prints \"True\"\n"
       putStr "Proposition: "
       hFlush stdout -- Write text to console immediately for Windows

       xs <- getLine
       case parse prop xs of
           [(n, [])]    -> putStrLn $ "Result: " ++ show (isTaut n)
           [(_, out)]   -> error ("unused input " ++ out)
           []           -> error "invalid input"

prop :: Parser Prop
prop = do l <- both
          do  symbol ">"
              r <- prop
              return $ l `Imply` r
           +++ return l

both :: Parser Prop
both = do l <- neg
          do symbol "&"
             r <- both
             return $ l `And` r
           +++ return l

neg :: Parser Prop
neg = do symbol "!"
         Not <$> factor
      +++ factor

factor :: Parser Prop
factor = do symbol "("
            p <- prop
            symbol ")"
            return p
          +++ tf

tf :: Parser Prop
tf = do symbol "True"
        return $ Const True
      +++ do symbol "False"
             return $ Const False
      +++ var

var :: Parser Prop
var = do v <- token upper
         return $ Var v
```



Main.hs

```haskell
module Main where

import C10P6

-- |Tests 'C10P6.tautChecker' for user-input boolean expression tautology checks
main :: IO ()
main = tautChecker
```



### Learn You a Haskell Website

#### Chapter 11, Applicative functors

I am not comfortable with my understanding of applicative functors nor `fmap`. It seems that functors are functions can be composed with other functions like mathematical $f\circ g(x)=f(g(x))$, and what makes applicative functors different than other functors is that you can apply other functions besides the original functor type. The main required Haskell operator to implement a functor appears to be `(<&)` which takes the  mathematical $g(x)$ and applies it to the arguments of existing $f(x)$ like so: `a -> f b -> f a`. For the functor to also be applicative, however, it must obey additional laws. So since the following code shows `Max` does not obey applicative functor law `pure f <*> pure x = pure (f x)` it is not truly an applicative functor.

Main.hs :

```haskell
module Main where

import Data.Semigroup
    ( Sum(..)
    , Max(..)
    )

{-|
    Tests various applicative functor laws on a couple applicative functors:
    Sum and Max
-}
main :: IO ()
main = do
    -- Passes : Applicative Functor Law, "Sum":
    --     pure f <*> x = fmap f x
    print $ show (getSum $ getSum $ fmapSum 42) ++ "==" ++
        show (getSum $ getSum $ pureSum 42)

    -- Passes : Applicative Functor Law, "Sum":
    --     pure id <*> v = v
    print $ show (getSum $ idSum 42) ++ "==" ++ show (getSum $ Sum 42)

    -- Passes : Applicative Functor Law, "Max":
    --     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    print $ show (getMax $ dotCompositionMax 42) ++ "==" ++
        show (getMax $ rightAssociatedMax 42)

    -- Partial failure (different number of getMax required, but same value):
    --     Applicative Functor Law, "Max" :
    --     pure f <*> pure x = pure (f x)
    print $ show (getMax $ getMax $ getMax $ puresMax 42) ++ "==" ++
        show (getMax $ getMax $ pureMax 42)

    -- Passes : Applicative Functor Law, "Max":
    --     u <*> pure y = pure ($ y) <*> u
    print $ show ((getMax $ getMax $ getMax $ rightPureMax (Max (*2))) 21) ++
        "==" ++ show ((getMax $ getMax $ getMax $ rightPureMax (Max (*2))) 21)

    where
    -- pure f <*> x = fmap f x
    fmapSum = fmap Sum
    pureSum = pure Sum <*> Sum

    -- pure id <*> v = v
    idSum = pure id <*> Sum
    -- Comparing with Sum data constructor itself (Sum :: a -> Sum a)

    -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    dotCompositionMax w = pure (.) <*> (Max id) <*> (Max id) <*> Max w
    rightAssociatedMax w = (Max id) <*> ((Max id) <*> Max w)

    -- Fails for Max, so Max is not an applicative functor??
    -- pure f <*> pure x = pure (f x)
    puresMax x = pure Max <*> pure $ Max x
    pureMax x = pure $ Max x

    -- u <*> pure y = pure ($ y) <*> u
    rightPureMax y = Max id <*> pure (Max y)
    leftPureMax y = pure ($ Max y) <*> Max id
```



#### Chapter 12, A knight's quest

C12.hs :

```haskell
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
```



Main.hs :

```haskell
module Main where

import C12

{-|
    Tests 'C12.movesToReachIn' to find all combinations of chess moves for a
    Knight to be able to move from a position to another position in n moves or
    less.
-}
main :: IO ()
main = print $ (6, 2) `movesToReachIn3` (6, 1)

    where
    movesToReachIn3 = movesToReachIn 3
```



#### Chapter 13, Error version of Walk the line

C13.hs :

```haskell
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
```



I tried for a while to get `StateT` and `ExceptT` in a transformer stack so I could reduce the repetition on `pole <- land<Left/Right> n pole` over and over. I wanted it to just be `land<Left/Right> n` since every operation returns the state we would want to store and reuse. Every combination of `ExceptT Pole (StateT Pole IO) Pole` or `StateT Pole (ExceptT Pole IO) Pole` didn't seem to work for me.

Main.hs :

```haskell
module Main where

import C13

import Control.Monad.Except
    ( runExceptT
    , lift
    )

{-|
    Tests 'landLeft' and 'landRight' in a 'runExceptT' which should return early
    when unbalanced. I could not figure out how to combine StateT and ExceptT to
    reduce repetition with `pole <- land<Left/Right> n pole` over and over and
    instead use just `land<Left/Right> n`.
-}
main :: IO ()
main = do
    failure <- failureTest
    printResult failure

    success <- successTest
    printResult success

    where
    failureTest = runExceptT $ do
        pole <- landLeft 3 pole     -- 3,   0
        pole <- landRight 6 pole    -- 3,   6
        pole <- landLeft 5 pole     -- 8,   6
        pole <- landRight 6 pole    -- 8,   12 -- unbalanced

        lift $ print "Never gets this far"
        landLeft 3 pole

    successTest = runExceptT $ do
        pole <- landLeft 3 pole     -- 3,   0
        pole <- landRight 6 pole    -- 3,   6
        pole <- landLeft 5 pole     -- 8,   6
        pole <- landRight 5 pole    -- 8,   11 -- unbalanced
        landLeft 3 pole             -- 11,  11
        -- Makes it all the way to the end this time

    printResult result = print $ (case result of
        Left _  -> "Tightroper fell"
        Right _ -> "Balanced to the end")
        ++ " with pole: " ++ either show show result

    -- Initial state
    pole = (0, 0)
```

