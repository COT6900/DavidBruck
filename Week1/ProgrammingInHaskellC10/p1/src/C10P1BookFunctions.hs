module C10P1BookFunctions
    ( nat2int
    , int2nat
    , add
    , Nat (..)
    ) where

-- |Book reference data type 'Nat'
data Nat = Zero | Succ Nat

-- |Book reference function 'nat2int'
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

-- |Book reference function 'int2nat'
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n - 1)

-- |Book reference function 'add'
add Zero n      = n
add (Succ m) n  = Succ $ add m n
