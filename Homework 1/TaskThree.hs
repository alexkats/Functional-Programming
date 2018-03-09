module TaskThree where

import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

mergeSort :: [Int] -> [Int]
mergeSort l@(x : []) = l
mergeSort list       = merge (mergeSort a) $ mergeSort b
  where
    (a, b) = splitAt (quot (length list) 2) list

merge :: [Int] -> [Int] -> [Int]
merge [] []             = []
merge x []              = x
merge [] y              = y
merge (x : xs) (y : ys) = if x < y
                          then (x : (merge xs (y : ys)))
                          else (y : (merge (x : xs) ys))
