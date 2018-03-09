module TaskTwo where

import Data.Bits (setBit)
import Data.List (nub)

-- Как в прошлом задании, только реализация может занимать произвольное число строк,
-- хотя для максимального балла требуется наиболее короткая (форматирование и длина
-- имён переменных не учитывается). Решение должно быть асимптотически эффективным,
-- причем скрытую константу в асимптотике тоже желательно уменьшить, насколько это
-- возможно.
--
-- Необходимо проверить, что переданное число содержит каждую цифру 0..9 хотя бы раз.

hasAllDigits :: Int -> Bool
hasAllDigits n = hasAll n 0

hasAll :: Int -> Int -> Bool
hasAll n 1023 = True
hasAll 0 i    = False
hasAll n i    = hasAll (n `div` 10) (setBit i (n `mod` 10))

hasAllDigits' :: Int -> Bool
hasAllDigits' n = len n == 10
  where
    len = length . nub . convertToList

convertToList :: Int -> [Int]
convertToList 0 = []
convertToList n = (n `mod` 10) : convertToList (n `div` 10)
