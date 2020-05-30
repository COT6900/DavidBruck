module Main where

import qualified C2P4 ( last )

{-|
    Tests 'C2P4.last' twice:
        once with a hardcoded list,
        and once with an empty list (throws an error)
-}
main :: IO ()
main = do {
    print (valueOrError ["primero", "luego", "finalmente"]);

    -- cannot take last of empty list
    print (valueOrError ([] :: [String]))
}
    where valueOrError possible = case C2P4.last possible of
              Nothing       -> errorWithoutStackTrace "empty list"
              Just possible -> possible
