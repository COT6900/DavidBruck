{-|
Module      : C8P6 and P7
Description : Programming in Haskell, chapter 8, problems 6 & 7
Maintainer  : David Bruck
-}
module C8P6
    ( eval
    ) where

{-
    The starting code which was able to parse expressions with (+) / (*)
    was provided by the book. The assignment problem is to add (-) / (/)
    support, but the book also says to download the source to extend the
    parser from the book's website:
    https://www.cs.nott.ac.uk/~pszgmh/pih.html

    The downloaded code for the starting parser already has implemented
    (-) / (/) support, so instead I will solve a different problem. I will
    be adding support for operators (√) / (^) for square root / power.
    (sqrt()) will also be an equivalent syntax for square root.
    - David Bruck

    -- modifications - David Bruck (search "modification" without quotes)
-}

import Parsing

-- modification: entire 'symbols' function - David Bruck
-- |Parser which accepts any of a list of string alternates
symbols :: [String] -> Parser String
symbols alternates = token $ strings alternates where
    strings alts = if any (not . null) alts
                   then do x <- item
                           case filter ((==x) . head) alts of
                               []        -> empty
                               remaining -> strings $ map tail remaining
                   else return []

expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           +++ do symbol "-" -- Problem 6 as answered by book authors
                  e <- expr
                  return (t - e)
           +++ return t

-- modification: factor to power; and "f" to "p" - David Bruck
term :: Parser Int
term = do p <- power
          do symbol "*"
             t <- term
             return (p * t)
           +++ do symbol "/" -- Problem 6 as answered by book authors
                  t <- term
                  return (p `div` t)
           +++ return p

-- modification: entire 'power' function - David Bruck
-- |Parser allowing symbol (↑) or (^) for "to the power of"
power :: Parser Int
power = do s <- radicand
           -- Problem 7 - David Bruck
           do symbols ["^", "↑"]
              s' <- radicand
              return $ s ^ s'
            +++ return s

-- modification: entire 'radicand' function - David Bruck
-- |Parser allowing symbol (√) or "sqrt"-equivalent for square root
radicand :: Parser Int
           {-
               Problem of my own making since I could not do problem 6
               since the book-provided source already completed it
               - David Bruck
           -}
radicand = do symbols ["sqrt", "√"]
              f <- factor
              return (floor $ sqrt $ fromIntegral f :: Int)
            +++ factor

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          +++ integer

eval :: String -> Int
eval xs = case parsed of
              [(n, [])] -> n
              [(_, out)] -> error ("unused input " ++ out)
              [] -> error "invalid input"
              where parsed = parse expr xs
