{-|
Module      : C10P4
Description : Programming in Haskell, chapter 10, problem 4
Maintainer  : David Bruck
-}
module C10P4
    ( Tree
    , balance
    ) where

import Data.List
    ( sort
    , genericSplitAt
    )

-- |Binary tree which allows everything except a node with only a right child
data Tree a
    = Empty
    | Leaf a
    | LeftOnlyNode a (Tree a)
    | FullNode a (Tree a) (Tree a)

-- |Takes a possibly unsorted list and turns it into a balanced 'Tree'
balance xs = balanceImpl (sort xs)

spacesPerTab = 2
{-
    Show on ordered Tree (FullNode 3 (LeftOnlyNode 2 (Leaf 1)) (Leaf 4)) prints:
    3
      2
        1
      4
    (prints current node/leaf value,
     then travels down left (if node),
     then travels down right (if full node);
     each level is intended 2 extra spaces)

    Show on empty Tree prints: empty
-}
instance (Show a) => Show (Tree a) where
    show Empty  = "empty" {- Only the entire tree can be Empty (special case),
                             Empty is not reused for leafs of other nodes
                             so we never have to check for it on tree-walking
                          -}

    -- Start at zero indendation and without a starting newline
    show a      = showImpl 0 a where
        -- Abstract state machine to perform tree-walking
        showImpl t a = case a of
            (Leaf val)                  -> showVal val
            (LeftOnlyNode val left)     -> showValAndLeft val left
            (FullNode val left right)   -> showValFull val left right
            where

            showValFull val left right  = showValAndLeft val left ++
                                          showImpl (t + 1) right
            showValAndLeft val left     = showVal val ++ showImpl (t + 1) left
            showVal val                 = (if t == 0 then "" else "\n") ++
                                          replicate (t * spacesPerTab) ' ' ++
                                          show val

-- |Internal implementation of 'balance' which requires a sorted list
balanceImpl :: [a] -> Tree a
balanceImpl []      = Empty
balanceImpl [a]     = Leaf a
balanceImpl [a, b]  = LeftOnlyNode b (Leaf a)
balanceImpl xs      = {-
                          1. Split the list in half favoring a longer right side
                          2. Make the parent out of the head of the right part,
                          3. Make the left child out of left and right child out
                             of right's tail
                      -}
                      FullNode
                          (head right)
                          (balanceImpl left)
                          (balanceImpl (tail right)) where

                          (left, right) = genericSplitAt medianIndex xs
                          medianIndex   = length xs `quot` 2
