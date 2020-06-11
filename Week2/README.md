### David Bruck COT6900 - Week 2

### Programming in Haskell, chapter 4

#### Problem 2

##### (a)

C4P2.hs :

```haskell
{-|
Module      : C10P4
Description : Programming in Haskell, chapter 4, problem 2
Maintainer  : David Bruck
-}
module C4P2
    ( safetail
    ) where
    
-- (a) conditional expression

-- |Like 'tail' except returns an empty list for an empty input list
safetail xs = if null xs
              then []
              else tail xs
```



Main.hs :

```haskell
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
```



##### (b)

```haskell
-- (b) guarded equations
safetail xs | null xs   = []
            | otherwise = tail xs
```



##### (c)

```haskell
-- (c) pattern matching
safetail []         = []
safetail (_ : xs)   = xs
```

Isn't `case a of ...` also considered "pattern matching"?

```haskell
-- (alternative pattern matching)
safetail xs = case xs of
                  (_ : xxs) -> xxs
                  _         -> []
```

Also, if you build with compiler option `-XLambdaCase`:

```haskell
safetail = \case
               (_ : xs)  -> xs
               _         -> []
```



### Programming in Haskell, chapter 5

#### Problem 3

C5P3.hs :

```haskell
{-|
Module      : C5P3
Description : Programming in Haskell, chapter 5, problem 3
Maintainer  : David Bruck
-}
module C5P3
    ( pyths
    ) where

{-|
    Finds all combinations of natural numbers (x, y, z)
    where the max of (x, y, z) = z
        and z <= the 'max' parameter
        and x^2 + y^2 = z^2 (pythagorean)
-}
pyths max = filter (\(x, y, z) -> x * x + y * y == z * z) allCombinations

    where
    -- 1^2 + 1^2 > 1^2, so z > 1; similarly, x < z && y < z;
    -- also, allows for x == y, but I'm not sure that's possible?
    allCombinations = concatMap
        (\z -> [(x, y, z) | x <- [1..z - 1], y <- [1..z - 1]])
        [2..max] -- z
```



Main.hs :

```haskell
module Main where

import C5P3
import Text.Printf
    ( printf
    )
import Data.List
    ( intercalate
    )
import Data.Function
    ( (&)
    )

{-|
    Tests 'C5P3.pyths' to generate all pythagorean combinations
        (x^2 + y^2 = z^2) for z <= max (hardcoded for testing at 10)
-}
main :: IO ()
main = map
           (\(x, y, z) -> printf "(%d, %d, %d)" x y z)
           (pyths (10 :: Int)) {- pyths has polymorphic input,
                                  but '10' is apparently too ambiguous
                               -}
       & intercalate ",\n  "
       & printf "[ (x, y, z), ... ]:\n[ %s ]"
```



#### Problem 5

C5P5.hs :

```haskell
{-|
Module      : C5P5
Description : Programming in Haskell, chapter 5, problem 5
Maintainer  : David Bruck
-}
module C5P5
    ( combinedGenerators
    ) where

{-|
    Alternate implementation of single comprehension:
    [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
    except as a comprehension between two generators
-}
combinedGenerators = zip

    {- xs strategy: replicate each element
       ys strategy: replicate entire list
    
       zip result expected to produce all combinations:
       (x0 (x0 (x0 (x1 (x1 (x1 (x2 (x2 (x2
        y0),y1),y2),y0),y1),y2),y0),y1),y2)
    -}

    (concatMap (replicate $ length xs) xs)
    (concat (replicate (length xs) ys))

    where
    xs = [1, 2, 3]
    ys = [4, 5, 6]
```



Main.hs :

```haskell
module Main where

import C5P5
import Text.Printf
    ( printf
    )
import Data.List
    ( intercalate
    )
import Data.Function
    ( (&)
    )

{-|
    Tests 'C5P5.combinedGenerators' alternate implementation with two
    generators instead of just the book-provided one
-}
main :: IO ()
main = map
           (uncurry $ printf "(%d, %d)")
           combinedGenerators
       & intercalate ",\n  "
       & printf "[ (x, y), ... ]:\n[ %s ]"
```



### Programming in Haskell, chapter 6

#### Problem 4

C6P4.hs :

```haskell
{-|
Module      : C6P4
Description : Programming in Haskell, chapter 6, problem 4
Maintainer  : David Bruck
-}
module C6P4
    ( merge
    ) where

{-|
    Assuming both list inputs are sorted,
    'merge' concatenates elements to form one sorted list
-}
{- main case first: take each lowest element and
   append to the merge of everything else
-}
merge (x : xs)  (y : ys)    | x < y     = x : merge xs          (y : ys)
                            | otherwise = y : merge (x : xs)    ys
merge xs        ys          = xs ++ ys {- one or both lists are empty;
                                          and the other one's sorted
                                       -}
```



Main.hs :

```haskell
module Main where

import C6P4

{-|
    Tests 'C6P4.merge' to merge two sorted input lists into
    a sorted output with all elements
-}
main :: IO ()
main = print $ show $ merge [1, 2, 4, 5, 6] [3, 7, 8, 9, 10]
```



#### Problem 5

C6P5.hs :

```haskell
{-|
Module      : C6P5
Description : Programming in Haskell, chapter 6, problem 5
Maintainer  : David Bruck
-}
module C6P5
    ( msort
    ) where

import C6P4

{-
    Implements merge sort by using 'C6P4.merge' (which retains order)
    on two halves which are themselves recursively merge-sorted
-}
msort []    = []
msort [x]   = [x]
msort xs    = merge (msort left) (msort right)
              
              where
              (left, right) = splitAt (length xs `div` 2) xs
```



Main.hs :

```haskell
module Main where

import C6P5

{-|
    Tests 'C6P5.msort' to sort a list by merging msort-ed sublists
-}
main :: IO ()
main = print $ show $ msort [9, 1, 8, 2, 7, 3, 6, 4, 5]
```



### Programming in Haskell, chapter 7

#### Problem 7

Functions we are copying from the book verbatim, C7P7BookFunctions.hs :

```haskell
module C7P7BookFunctions
    ( bookChop8
    , bookMap
    , unfold
    ) where

-- |Book reference function chop8
bookChop8 []    = []
bookChop8 bits  = take 8 bits : bookChop8 (drop 8 bits)

-- |Book reference function map
bookMap f xs    = [f x | x <- xs]

{-|
    The 'unfold' function from the book,
    to be used as a basis of the unfold function versions
-}
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)
```



C7P7.hs :

```haskell
{-|
Module      : C7P7
Description : Programming in Haskell, chapter 7, problem 7
Maintainer  : David Bruck
-}
module C7P7
    ( unfoldChop8
    , unfoldMap
    , unfoldIterate
    ) where

import C7P7BookFunctions
    ( unfold
    )

-- |Custom implementation of function chop8 using 'unfold'
unfoldChop8     = unfold null           (take 8)    (drop 8)

-- |Custom implementation of function map using 'unfold'
unfoldMap f     = unfold null           (f . head)  tail

-- |Custom implementation of function iterate using 'unfold'
unfoldIterate   = unfold (const False)  id
```



Main.hs :

```haskell
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
```



### Programming in Haskell, chapter 8

#### Problem 6 and 7

Important note: the assignment problem 6 asked to implement new operators (-) and (/), but the book's supplementary source code referenced by the book already included this functionality in its calculator.hs. Instead I decided to add additional functionality instead: I added support for integer square roots, and I added support for alternate symbols to be treated equivalently. For example, you can perform a square root with (sqrt) or (√) via my new `symbols` function. Also, for problem 7, the book wanted a symbol (↑) for "to the power of", so for that one I treat (^) as equivalent with `symbols`.

Functions from a supplementary source code file of the same name from download link referenced by the book that I modified slightly (search "modification" without quotes), Parsing.hs :

```haskell
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Mostly unmodified from original source except for the noted modifications.
-- Original source downloaded from website:
-- https://www.cs.nott.ac.uk/~pszgmh/pih.html
--
-- Noted modifications - David Bruck (search "modification" without quotes)

{-# LANGUAGE LambdaCase #-} -- modification to make hlint happy - David Bruck

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\case {-
                    modification to lambda-case to make hlint happy
                    - David Bruck
                -}
             []     -> []
             (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   -- modification to remove lambda to make hlint happy - David Bruck
   empty = P $ const []

   {-
      modification from provided source (book uses +++, source used <|>)
      - David Bruck
   -}
   (<|>) = (+++)

{-
   modification: moved definition from instance Alternative outside
   so 'expr' can use it; also, book uses +++, source used <|>
-}
(+++) :: Parser a -> Parser a -> Parser a
(+++) p q = P (\inp -> case parse p inp of
                        []        -> parse q inp
                        [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       +++ nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)
```



Functions starting from book supplementary source code file named calculator.hs which originally supported (+) / (*) / (-) / (/). I added support for alternates (√) or (sqrt) for integer square root (not in the assignment), and I added support for (^) or (↑) for "to the power of" (problem 7), C8P6.hs :

```haskell
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
```



Main.hs :
$$
\frac{2\cdot254+4}{\sqrt{2^2}}-2^{\sqrt 4\cdot 4}=\\
\frac{508+4}{2}-2^8=\\
256-256=0
$$

```haskell
module Main where

import C8P6

-- |Tests 'C8P6.eval' with a hardcoded equation with expected result: 0
main :: IO ()
-- √ == sqrt
-- ↑ == ^ ("to the power of")
main = print $ eval "(2*254 + 4)/sqrt(2↑2) - 2^( √4*4 )"
```

