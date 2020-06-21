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
eval _ (Val n)                  = [n | n > 0]
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
