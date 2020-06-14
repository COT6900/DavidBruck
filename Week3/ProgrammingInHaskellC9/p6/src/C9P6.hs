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
