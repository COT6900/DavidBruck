module Main where

import C7P7BookFunctions
    ( bookChop8
    , bookMap
    )
import C7P7
    ( unfoldChop8
    , unfoldMap
    , unfoldIterate
    )
import Data.Char
    ( isDigit
    )

{-
    Compares reference functions chop8 (from the book), map (from the book),
    and 'iterate' (from prelude) with custom functions which should perform
    the same except use 'unfold' (from the book)
-}
main :: IO ()
main = do {
    putStrLn "[function_name] [book_reference]==[unfold_version]";
    putStrLn $ "chop8 "
        ++ show (bookChop8 abcEncodedBits)
        ++ "==" ++ show (unfoldChop8 abcEncodedBits);
    putStrLn $ "map "
        ++ show (bookMap isDigit a1b2)
        ++ "==" ++ show (unfoldMap isDigit a1b2);
    putStrLn $ "iterate "
        ++ show (bin2int $ iterate (*2) 1)
        ++ "==" ++ show (bin2int $ unfoldIterate (*2) 1);
}
    where
    abcEncodedBits  = [ 1, 0, 0, 0, 0, 1, 1, 0,  -- first 8
                        0, 1, 0, 0, 0, 1, 1, 0,  -- second 8
                        1, 1, 0, 0, 0, 1, 1, 0 ] -- third 8
    a1b2            = "a1b2" -- non-digit, digit, non-digit, digit

    -- 1101 in binary is [1, 0, 1, 1] little-endian bits, and 13 in decimal
    {-|
        Book reference function 'bin2int', except with hardcoded bits
        and weights provided by a function so we can stub 'iterate'
    -}
    bin2int weights = sum [w * b | (w, b) <- zip weights [1, 0, 1, 1]]
