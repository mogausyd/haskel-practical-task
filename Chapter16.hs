module Main where

import Data.Char (toUpper)
import Data.List (group, sort)

----------------------------------------------
-- HC16T1: Reverse a String
----------------------------------------------
reverseString :: String -> String
reverseString = reverse

----------------------------------------------
-- HC16T2: Palindrome Checker
----------------------------------------------
isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
  where cleaned = [toUpper c | c <- str, c /= ' ']

----------------------------------------------
-- HC16T3: Factorial
----------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

----------------------------------------------
-- HC16T4: Filter Even Numbers
----------------------------------------------
filterEvens :: [Int] -> [Int]
filterEvens = filter even

----------------------------------------------
-- HC16T5: Uppercase String
----------------------------------------------
toUpperCase :: String -> String
toUpperCase = map toUpper

----------------------------------------------
-- HC16T6: nth Fibonacci Number
----------------------------------------------
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

----------------------------------------------
-- HC16T7: Element Existence in List
----------------------------------------------
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

----------------------------------------------
-- HC16T8: Insertion Sort
----------------------------------------------
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

----------------------------------------------
-- HC16T9: Remove Duplicates from List
----------------------------------------------
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise   = x : removeDuplicates xs

----------------------------------------------
-- HC16T10: Character Frequency in String
----------------------------------------------
charFrequency :: String -> [(Char, Int)]
charFrequency str = [(head x, length x) | x <- group (sort str)]

----------------------------------------------
-- MAIN: Demonstration of All Tasks
----------------------------------------------
main :: IO ()
main = do
  putStrLn "==== Haskell Chapter 16: Fundamental Functions ===="

  putStrLn "\nHC16T1: Reverse a String"
  print (reverseString "Haskell")

  putStrLn "\nHC16T2: Palindrome Checker"
  print (isPalindrome "madam")
  print (isPalindrome "hello")

  putStrLn "\nHC16T3: Factorial"
  print (factorial 5)

  putStrLn "\nHC16T4: Filter Even Numbers"
  print (filterEvens [1..10])

  putStrLn "\nHC16T5: Uppercase String"
  print (toUpperCase "functional programming")

  putStrLn "\nHC16T6: nth Fibonacci Number"
  print (fibonacci 10)

  putStrLn "\nHC16T7: Element Existence in List"
  print (elementExists 3 [1,2,3,4])
  print (elementExists 10 [1,2,3,4])

  putStrLn "\nHC16T8: Insertion Sort"
  print (insertionSort [5,2,8,1,3])

  putStrLn "\nHC16T9: Remove Duplicates from List"
  print (removeDuplicates [1,2,3,2,1,4,4,5])

  putStrLn "\nHC16T10: Character Frequency in String"
  print (charFrequency "haskell")
