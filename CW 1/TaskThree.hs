module TaskThree where

-- Реализовать структуру данных.
--
-- Реализовать циклический список, с операцией shift, возвращающей циклический список
-- со сдвинутым на 1 элементом вправо. Определите оператор !!! получения значения
-- списка по индексу.

data CyclicList a = CyclicList [a]

shift :: CyclicList a -> CyclicList a
shift (CyclicList []) = CyclicList []
shift (CyclicList l)  = CyclicList (last l : init l)
(!!!) :: CyclicList a -> Int -> a
(!!!) (CyclicList (x : _)) 0  = x
(!!!) (CyclicList (_ : xs)) n = (CyclicList xs) !!! (n - 1)
