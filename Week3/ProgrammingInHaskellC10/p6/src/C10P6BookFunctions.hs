{-|
    Boolean logic tautology checker code adapted from Programming in Haskell,
    chapter 10, Graham Hutton, Cambridge University Press, 2007
-}
module C10P6BookFunctions
    ( isTaut
    , Prop(..)
    ) where

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

type Assoc k v  = [(k, v)]
type Subst      = Assoc Char Bool

find k t = head [v | (k', v) <- t, k == k']

eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q

vars (Const _)      = []
vars (Var x)       = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Imply p q)    = vars p ++ vars q

---- Definition 1 of 'bools'
--bools n = map (map conv . make n . int2bin) [0..limit]
--
--    where
--    int2bin 0 = []
--    int2bin n = n `mod` 2 : int2bin (n `div` 2)
--
--    limit = 2 ^ n - 1
--
--    make n bits = take n (bits ++ repeat 0)
--
--    conv 0 = False
--    conv 1 = True

-- Definition 2 of 'bools'
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
    where bss = bools $ n - 1

substs p = map (zip vs) $ bools $ length vs
    where vs = rmdups $ vars p

rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/=x) xs)

isTaut p = and [eval s p | s <- substs p]
