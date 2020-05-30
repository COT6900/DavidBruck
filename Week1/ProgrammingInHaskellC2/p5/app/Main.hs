module Main where

import C2P5

-- |Tests 'C2P5.initOne' and 'C2P5.initTwo' with different hardcoded lists
main :: IO ()
main = do
    print (initOne ["notLast1", "notLast2", "last"])
    print (initTwo ["notLastA", "notLastB", "last"])
