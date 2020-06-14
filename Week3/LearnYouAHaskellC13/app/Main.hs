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
