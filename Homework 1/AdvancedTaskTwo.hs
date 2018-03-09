module TaskTwo where

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f list = map f $ transpose list

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose lists    = (map head lists) : transpose (map tail lists)

advancedZipN :: ([a] -> b) -> [[a]] -> [b]
advancedZipN f list = map f $ transpose list
