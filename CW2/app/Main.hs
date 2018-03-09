module Main where

import TaskOne
import TaskTwo

main :: IO ()
main = putStrLn ("Task 1: " ++ show testTask1) >> putStrLn ("Task 2: " ++ testTask2)

testTask1 = map maybeConcat [task1Test1, task1Test2, task1Test3, task1Test4]

task1Test1 = [Just [1, 2], Nothing, Just [2, 1]]
task1Test2 = [Nothing]
task1Test3 = [Nothing, Nothing]
task1Test4 = [Just [1, 3], Just [4, 9]]

testTask2 = solution
