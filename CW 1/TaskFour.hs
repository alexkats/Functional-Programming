module TaskFour where

-- Придумать и реализовать заданный тайпкласс.
--
-- Реализуйте инстанс класса типов Show для типа данных Matrix - обёртки
-- над списком списков (и сам тип данных тоже), - чтобы каждый список
-- выводился в отдельной строке.

data Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix []) = "[]"
    show (Matrix (x : xs)) = show x
                             ++
                             show (Matrix xs)
