module Main where

import C4P2
import Control.Exception
    ( catch
    , evaluate
    , SomeException
    )

{-|
    Compares Prelude function 'tail' with 'C4P2.safetail'.
    It is expected to provide equivalent results, except for an empty input.
-}
main :: IO ()
main = do
    print $ show (safetail twoElem) ++ "==" ++ show (tail twoElem)
    print $ show (safetail oneElem) ++ "==" ++ show (tail oneElem)
    
    -- To compare to 'tail' with 'empty',
    -- we have to catch the exception 'tail' throws
    tailEmpty <- evaluate (show $ tail empty) `catch` showEx
    print (show (safetail empty) ++ "<>" ++ tailEmpty) -- expected <>: not equal

    where
    twoElem = [1, 2]
    oneElem = [1]
    empty   = [] :: [Int]

    showEx :: SomeException -> IO String
    showEx ex = return $ show ex
    
