module TaskOne where

import           Data.Maybe (fromMaybe)

-- 1. Реализуйте требуемую функциональность из задания. Запрещается
-- использовать do-синтаксис.
--
-- Задача: Реализуйте функцию maybeConcat, которая принимает
-- [ Maybe [a] ] и возвращает конкатенацию списков внутри Just.

maybeConcat :: [ Maybe [a] ] -> [a]
maybeConcat = fromMaybe [] . foldr mappend Nothing
