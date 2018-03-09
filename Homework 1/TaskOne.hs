module TaskOne where

import           Data.Char (isDigit)

convertPlus :: String -> String
convertPlus ('+':b:xs) = if isDigit b
                         then (b:xs)
                         else "+"
convertPlus s = s

stringSum :: String -> Int
stringSum s = sum $ map (read . convertPlus) $ words s

tests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
        , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
        , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
        ]

test :: [(String, Int)]
test = zip tests $ map stringSum tests

advancedTests = ["+1", "1 +1", "-1 +1", "+1 -1"]

advancedTest :: [(String, Int)]
advancedTest = zip advancedTests $ map stringSum advancedTests
