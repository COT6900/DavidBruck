module Main where

import C11P4
import System.IO
    ( hFlush
    , stdout
    )
import Data.List
    ( intercalate
    )
import Data.List.Split
    ( chunksOf
    )

-- |Tests 'C11P4.countValidCombinations'
main :: IO ()
main = do putStrLn "Starting computation to find length of valid expressions,"
          putStrLn "except generalized for all integrals"
          putStrLn "(including zero / negative)"
          putStrLn "  This could take a while..."

          -- We are about to perform a time-consuming computation,
          -- so we want to make sure the warning makes it to the console
          -- before the program becomes unresponsive
          hFlush stdout

          -- The 'True' is for problem 5 allowing all integrals
          putStrLn $ "Valid expressions: " ++
              commaSeparate (show (countValidCombinations True numbers))

       where
       numbers = [1, 3, 7, 10, 25, 50] :: [Integer]

       {-
           Ability to comma-separate numbers by thousands places adapted from
           a StackOverflow answer by user Jonno_FTW to question:
           https://stackoverflow.com/questions/3752898
       -}
       commaSeparate xs = reverse $ intercalate "," $ chunksOf 3 $ reverse xs
