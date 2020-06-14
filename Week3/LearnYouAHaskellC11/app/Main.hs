module Main where

import Data.Semigroup
    ( Sum(..)
    , Max(..)
    )

{-|
    Tests various applicative functor laws on a couple applicative functors:
    Sum and Max
-}
main :: IO ()
main = do
    -- Passes : Applicative Functor Law, "Sum":
    --     pure f <*> x = fmap f x
    print $ show (getSum $ getSum $ fmapSum 42) ++ "==" ++
        show (getSum $ getSum $ pureSum 42)

    -- Passes : Applicative Functor Law, "Sum":
    --     pure id <*> v = v
    print $ show (getSum $ idSum 42) ++ "==" ++ show (getSum $ Sum 42)

    -- Passes : Applicative Functor Law, "Max":
    --     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    print $ show (getMax $ dotCompositionMax 42) ++ "==" ++
        show (getMax $ rightAssociatedMax 42)

    -- Partial failure (different number of getMax required, but same value):
    --     Applicative Functor Law, "Max" :
    --     pure f <*> pure x = pure (f x)
    print $ show (getMax $ getMax $ getMax $ puresMax 42) ++ "==" ++
        show (getMax $ getMax $ pureMax 42)

    -- Passes : Applicative Functor Law, "Max":
    --     u <*> pure y = pure ($ y) <*> u
    print $ show ((getMax $ getMax $ getMax $ rightPureMax (Max (*2))) 21) ++
        "==" ++ show ((getMax $ getMax $ getMax $ rightPureMax (Max (*2))) 21)

    where
    -- pure f <*> x = fmap f x
    fmapSum = fmap Sum
    pureSum = pure Sum <*> Sum

    -- pure id <*> v = v
    idSum = pure id <*> Sum
    -- Comparing with Sum data constructor itself (Sum :: a -> Sum a)

    -- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
    dotCompositionMax w = pure (.) <*> (Max id) <*> (Max id) <*> Max w
    rightAssociatedMax w = (Max id) <*> ((Max id) <*> Max w)

    -- pure f <*> pure x = pure (f x)
    puresMax x = pure Max <*> pure $ Max x
    pureMax x = pure $ Max x

    -- u <*> pure y = pure ($ y) <*> u
    rightPureMax y = Max id <*> pure (Max y)
    leftPureMax y = pure ($ Max y) <*> Max id
