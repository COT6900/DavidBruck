module C10P1BookFunctions
    ( value
    , Expr (..)
    ) where

-- |Book reference data type 'Expr'
data Expr = Val Integer | Add Expr Expr
-- |Book reference function 'value'
value (Val n)   = n
value (Add x y) = value x + value y

-- |Book reference type 'Cont'
type Cont   = [Op]
-- |Book reference data type 'Op'
data Op     = EVAL Expr | ADD Integer

-- |Book reference function 'eval'
eval (Val n) c      = exec c n
eval (Add x y) c    = eval x $ EVAL y : c

-- |Book reference data type 'exec'
exec [] n           = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m  = exec c (n + m)
