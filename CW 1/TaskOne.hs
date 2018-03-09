module TaskOne where

import Data.List (group)

-- Написать функцию в две строки: тип в наиболее общем виде и реализация. Реализация
-- должна быть максимально короткой. Нельзя использовать другие пакеты кроме Prelude
-- Data.List, Data.Char, Data.Map, Data.Tree.
--
-- Верните список, в котором все подряд идущие одинаковые элементы заменены на один.

distinct :: Eq a => [a] -> [a]
distinct = map head . group
