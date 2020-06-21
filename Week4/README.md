### David Bruck COT6900 - Week 4

### Programming in Haskell, chapter 11

#### Problems 4 and 5

Functions we are copying from the book with some noted modifications, C11P4BookFunctions.hs :

```haskell
{-|
    "The countdown problem" code adapted from Programming in Haskell,
    chapter 11, Graham Hutton, Cambridge University Press, 2007

    Modifications by David Bruck
    See "modification" without the quotes
    Also, most of the type signatures were removed to allow Integral (Problem 5)
-}
{-# LANGUAGE GADTs #-}
module C11P4BookFunctions
    ( Op(..)
    , Expr(..)
    , choices
    , exprs
    , eval
    , solution -- Not actually used for problem 4 or 5
    ) where

data Op = Add | Sub | Mul | Div

{-
    Problem 5: increasing numeric domain to arbitrary integers
    modification: generalized algebraic data types so it works with any integrals
                  -David
-}
data Expr a where
    Val :: Integral a => a -> Expr a
    App :: Integral a => Op -> Expr a -> Expr a -> Expr a

{-
    Problem 5: allowing for zero and negatives via 'generalized' option
    modification: generalized means allow all subtraction, but also have to add
                  a condition to not divide by zero
                  -David
-}
valid _ Add _ _             = True
valid generalized Sub x y   = generalized || x > y
valid _ Mul _ _             = True
valid _ Div x y             = y /= 0 && x `mod` y == 0

apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

-- modification: generalized integrals includes negatives for problem 5 -David
eval generalized (Val n)        = [n | generalized || n > 0]
eval generalized (App o l r)    = [apply o x y | x <- eval generalized l,
                                                 y <- eval generalized r,
                                                 valid generalized o x y]

subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave x []     = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y:) (interleave x ys)

perms = foldr (concatMap . interleave) [[]]

{-
    Problem 5: increasing numeric domain to arbitrary integers
    modification: type signature now required since [any] -> [[any]] was not as
                  restrictive as (Integral a) required by 'Expr' data type
                  -David
-}
choices :: Integral a => [a] -> [[a]]
choices xs = concatMap perms $ subs xs

-- modification: generalized integrals includes negatives for problem 5 -David
solution generalized e ns n = elem (values e) (choices ns) || eval generalized e == [n]

split []        = []
split [_]       = []
split (x:xs)    = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs []    = []
exprs [n]   = [Val n]
exprs ns    = [e | (ls, rs) <- split ns,
                   l <- exprs ls,
                   r <- exprs rs,
                   e <- combine l r]

combine l r = [App o l r | o <- ops]

ops = [Add, Sub, Mul, Div]
```



C11P4.hs :

```haskell
{-|
Module      : C11P4
Description : Programming in Haskell, chapter 11, problems 4 and 5
Maintainer  : David Bruck
-}
module C11P4
    ( countExpressionCombinations
    , countValidCombinations
    ) where

import C11P4BookFunctions
    ( Expr
    , exprs
    , choices
    , eval
    )

expressionCombinations numbers = map exprs $ choices numbers

-- |Counts all possible combinations of expressions for a set of input numbers
countExpressionCombinations numbers =
    sum $ map length $ expressionCombinations numbers

-- |Counts all valid expressions that can be formed for a set of input numbers
countValidCombinations generalized numbers =
    sum $ map (length . validExpressions) $ expressionCombinations numbers

    where
    {-
        generalized of 'False' is for problem 4 limiting on positive Int;
        while 'True' is for problem 5 generalizing to include all integrals
        (including zero and negative)
    -}
    validExpressions = filter (not . null . eval generalized)
```



Problem 4, Main.hs :

```haskell
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

-- |Tests 'C11P4.countExpressionCombinations' and 'C11P4.countValidCombinations'
main :: IO ()
main = do putStrLn $ "Starting computation to find length of all possible " ++
                     "and valid expressions."
          putStrLn "  This could take a while..."

          -- We are about to perform a time-consuming computation,
          -- so we want to make sure the warning makes it to the console
          -- before the program becomes unresponsive
          hFlush stdout

          putStrLn $ "Possible expressions: " ++
              commaSeparate (show (countExpressionCombinations numbers))

          -- The 'False' is for problem 4 limiting on natural (positive) Int
          putStrLn $ "Valid expressions: " ++
              commaSeparate (show (countValidCombinations False numbers))

       where
       numbers = [1, 3, 7, 10, 25, 50] :: [Int]

       {-
           Ability to comma-separate numbers by thousands places adapted from
           a StackOverflow answer by user Jonno_FTW to question:
           https://stackoverflow.com/questions/3752898
       -}
       commaSeparate xs = reverse $ intercalate "," $ chunksOf 3 $ reverse xs
```



Problem 5, Main.hs :

```haskell
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
```



### Programming in Haskell, chapter 13

#### Problem 2

**Note:** inductive reasoning does not display well on GitHub.com since it does not have a mathematic $\LaTeX$ plugin. I will be providing PDFs rendered from [Typora](https://typora.io/), which can also be used to view the README.md.

Book definitions:

```haskell
data Nat = Zero | Succ Nat
add Zero m = m
add (Succ n) m = Succ (add n m)
```



Show that: `add n (Succ m) = Succ (add n m)`

Base case #1 ($n=Zero$):
$$
\begin{equation}
\begin{split}
&add\text{ }Zero\text{ }(Succ\text{ }m)\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying the outer }add\text{ }\}\\

&Succ\text{ }m\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ unapplying }add\text{ }\}\\

&Succ\text{ }(add\text{ }Zero\text{ }m)\text{ }\larr\text{ equal to right-hand side}
\end{split}
\end{equation}
$$


Base case #2 ($m=Zero$), left-hand side:
$$
\begin{equation}
\begin{split}
&add\text{ }n\text{ }(Succ\text{ }Zero)\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying the outer }add\text{ }\}\\

&Succ\text{ }n
\end{split}
\end{equation}
$$
Base case #2 ($m=Zero$), right-hand side:
$$
\begin{equation}
\begin{split}
&Succ\text{ }(add\text{ }n\text{ }Zero)\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying the inner }add\text{ }\}\\

&Succ\text{ }n\text{ }\larr\text{ equal to converted left-hand side}
\end{split}
\end{equation}
$$


Inductive case ($Succ\text{ }n$), left-hand side:
$$
\begin{equation}
\begin{split}
&add\text{ }(Succ\text{ }n)\text{ }(Succ\text{ }m)\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }add\text{ }\}\\

&Succ\text{ }(add\text{ }n\text{ }(Succ\text{ }m))\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ induction hypothesis }\}\\

&Succ\text{ }(Succ\text{ }(add\text{ }n\text{ }m))
\end{split}
\end{equation}
$$
Inductive case ($Succ\text{ }n$), right-hand side:
$$
\begin{equation}
\begin{split}
&Succ\text{ }(add\text{ }(Succ\text{ }n)\text{ }m)\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying inner }add\text{ }\}\\

&Succ\text{ }(Succ\text{ }(add\text{ }n\text{ }m))\text{ }\larr\text{ equal to converted left-hand side}
\end{split}
\end{equation}
$$



#### Problem 7

Book definitions:

```haskell
map f []		= []
map f (x:xs)	= f x : map f xs
(f . g) x		= f (g x)
```



Show that: `map f (map g xs) = map (f . g) xs`

Base case, totally empty ($xs=[\text{ }]$), left-hand side:
$$
\begin{equation}
\begin{split}
&map\text{ }f\text{ }(map\text{ }g\text{ }[\text{ }])\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying the inner }map\text{ }\}\\

&map\text{ }f\text{ }[\text{ }]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }map\text{ }\}\\

&[\text{ }]
\end{split}
\end{equation}
$$
Base case, totally empty ($xs=[\text{ }]$), right-hand side:
$$
\begin{equation}
\begin{split}
&map\text{ }(f\text{ }.\text{ }g)\text{ }[\text{ }]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }map\text{ }\}\\

&[\text{ }]\larr\text{ equal to converted left-hand side}
\end{split}
\end{equation}
$$


Base case, one element ($(x:xs)=(x:[\text{ }])$), left-hand side:
$$
\begin{equation}
\begin{split}
&map\text{ }f\text{ }(map\text{ }g\text{ }[x])\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying the inner }map\text{ }\}\\

&map\text{ }f\text{ }[g\text{ }x]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }map\text{ }\}\\

&[f\text{ }(g\text{ }x)]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ unapplying }(f . g)\text{ }x\text{ }\}\\

&[(f\text{ }.\text{ }g)\text{ }x]
\end{split}
\end{equation}
$$
Base case, one element  ($(x:xs)=(x:[\text{ }])$), right-hand side:
$$
\begin{equation}
\begin{split}
&map\text{ }(f\text{ }.\text{ }g)\text{ }[x]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }map\text{ }\}\\

&[(f\text{ }.\text{ }g)\text{ }x]\larr\text{ equal to converted left-hand side}
\end{split}
\end{equation}
$$


Inductive case, $n$ elements ($(\text{ }...\text{ }:\text{ }x_{n-1}:x_n:xs)=[\text{ }...\text{ }:\text{ }(x_{n-1}:(x_n:[\text{ }]))]$):
$$
\begin{equation}
\begin{split}
&map\text{ }f\text{ }(map\text{ }g\text{ }[\text{ }...\text{ }:\text{ }(x_{n-1}:(x_n:[\text{ }]))])\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ applying }map\text{ twice per element }\}\\

&map\text{ }f\text{ }(map\text{ }g\text{ }[\text{ }...\text{ }:\text{ }x_{n-1}])\text{ ++ }map\text{ }f\text{ }(map\text{ }g\text{ }[x_n])\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ substitute base case for one element }\}\\

&map\text{ }f\text{ }(map\text{ }g\text{ }[\text{ }...\text{ }:\text{ }x_{n-1}])\text{ ++ }map\text{ }(f\text{ }.\text{ }g)\text{ }[x_n]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ apply recursively }n-1\text{ to }1\text{ }\}\\

&map\text{ }(f\text{ }.\text{ }g)\text{ }[x_1]\text{ ++ }\text{ }...\text{ ++ }map\text{ }(f\text{ }.\text{ }g)\text{ }[x_{n-1}]\text{ ++ }map\text{ }(f\text{ }.\text{ }g)\text{ }[x_n]\\

=\text{ }&\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }\text{ }
\{\text{ apply }++\text{  }\}\\

&map\text{ }(f\text{ }.\text{ }g)\text{ }[\text{ }...\text{ }:\text{ }(x_{n-1}:(x_n:[\text{ }]))]
\end{split}
\end{equation}
$$



### Learn You a Haskell Website

#### Chapter 8, Kinds and some type-foo

Haskell is able to infer types for parameters to functions (including operators) and type constructors by starting at special "any" types like "*" and applying restrictions. For example, a function taking one parameter as input, and outputting the parameter output:

```haskell
f :: forall a b. a -> b
```

We can read this function as for all generically-typed parameter whose type's name will be represented by `a`, take an input and output another unrestricted type `b` (but possibly also the same type as `a`). This does not help us reason what the function might do, but we might assume it actually transforms an input.



The first restriction we can place is return the same type output as the input, but the now singular type is still otherwise not restricted:

```haskell
f :: forall a. a -> a
```

A good example of a versatile function with this signature is `id` and works like `id anything == anything`.



However, the language cannot be built from completely non-restrictive functions alone and still be functional. For example, performing a mathematical operation to produce a numeric output might also have restrictions on numeric input. For example, if we tried to add with function `` `+` x y `` with parameters of `x` and `y` as themselves function types (`* -> *`), we could not expect Haskell to know what to do, maybe one could argue it could suddenly act like compositional function `` `.` f g ``, but instead, Haskell adds type restrictions to clarify. Function `` `+` x y `` is restricted like so:

```haskell
(+) :: forall a. (Num a) => a -> a -> a
```

Now, by reading the type signature alone, we know the function can take in two parameters which are type-restricted to be a sort of number (but specifically the same sort), and output another number. This is powerful, by the name `(+)` and type signature alone, we can infer how the function will behave.



Now combining this all together:

```
pointlessFunction x = id x
```

Haskell reads the functions used in the body of this new function `pointlessFunction` and decides `id` does not restrict the type of `x` any further from the default, so the overall function is typed `* -> *`.



However, if we make a new function like so:

```haskell
add2 = `+` 2
```

Haskell now sees function `` `+` x y `` in the function body, sees it is applied one of 2 of its parameters (specifically x) and returns a partially applied function ready to receive `y`. Also, we now can infer the resulting function's type (exactly how Haskell is able to be programmed with a powerful type inference system):

```haskell
add2 :: forall a. (Num a) => a -> a
```


Therefore, it is logical that Haskell simply reads through all the available information and sees if there is some combination of type restrictions which satisfies all the function bodies, and returns types which are as unrestricted as possible except for the minimal restrictions required. This is how we can only very rarely need to add type descriptions ourselves when writing code.