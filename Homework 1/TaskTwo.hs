module TaskTwo where

import           Data.List (transpose)

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f list = map f $ transpose list
