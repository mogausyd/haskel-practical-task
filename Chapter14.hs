{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE PartialTypeSignatures #-}

import qualified Data.List as L
import qualified Data.Map.Strict as M

-- HC14T5: Custom Result type with additional functionality
data Result a = Success a | Failure String deriving (Show, Eq)

-- Helper to check if Result is successful
isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False

-- HC14T8: Character frequency using Map for efficiency
charFrequency :: String -> [(Char, Int)]
charFrequency text = 
  L.sort $ M.toList $ M.fromListWith (+) [(ch, 1) | ch <- text]

-- Alternative implementation with explicit folding
countChars :: String -> [(Char, Int)]
countChars = L.sort . M.toList . foldr increment M.empty
  where
    increment ch = M.insertWith (+) ch 1

-- HC14T9: Partial type signature for safe list operations
firstElement :: [a] -> _
firstElement []     = Nothing
firstElement (x:xs) = Just x

-- Additional safe function with partial signature
secondElement :: [a] -> _
secondElement (_:x:_) = Just x
secondElement _       = Nothing

main :: IO ()
main = do
  putStrLn "=== HC14T1: Cabal Project Greeting ==="
  putStrLn "Welcome to the Cabal project!"
  
  putStrLn "\n=== HC14T2: Mock Random Value ==="
  let mockRand = 42
  putStrLn $ "Simulated random: " ++ show mockRand
  
  putStrLn "\n=== HC14T3: Numeric Underscores Demo ==="
  let largeNum = 1_000_000 :: Int
  let anotherNum = 5_000_250 :: Int
  putStrLn $ "Large number: " ++ show largeNum
  putStrLn $ "Another number: " ++ show anotherNum
  
  putStrLn "\n=== HC14T4: Type Applications ==="
  let parsed = read @Int "123"
  let parsed2 = read @Double "45.67"
  putStrLn $ "Parsed Int: " ++ show parsed
  putStrLn $ "Parsed Double: " ++ show parsed2
  
  putStrLn "\n=== HC14T5: Result Type Examples ==="
  let successCase = Success 99 :: Result Int
  let failureCase = Failure "Something went wrong" :: Result Int
  print successCase
  print failureCase
  putStrLn $ "Is success? " ++ show (isSuccess successCase)
  putStrLn $ "Is failure success? " ++ show (isSuccess failureCase)
  
  putStrLn "\n=== HC14T8: Character Frequency Analysis ==="
  let testWord = "mississippi"
  putStrLn $ "Analyzing: " ++ testWord
  print (charFrequency testWord)
  putStrLn "Alternative implementation:"
  print (countChars testWord)
  
  putStrLn "\n=== HC14T9: Partial Type Signatures ==="
  let numbers = [10, 20, 30] :: [Int]
  putStrLn $ "First element: " ++ show (firstElement numbers)
  putStrLn $ "Second element: " ++ show (secondElement numbers)
  putStrLn $ "Empty list first: " ++ show (firstElement ([] :: [Int]))
  
  putStrLn "\n=== HC14T10: Validation Tests ==="
  let expected = [('i',4), ('m',1), ('p',2), ('s',4)]
  let actual = charFrequency "mississippi"
  if actual == expected
    then putStrLn "✓ Character frequency test PASSED"
    else putStrLn $ "✗ Test FAILED: expected " ++ show expected ++ 
                    " but got " ++ show actual
