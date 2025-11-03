-- Haskell Chapter 12 Practical Tasks: Programs and Modules
import Data.List (sort)
import Data.Char (toLower)
import System.IO
import System.IO.Error (catchIOError)

-- HC12T2: Add Two Numbers
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- HC12T3: Factorial Function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome str = cleaned == reverse cleaned
  where cleaned = map toLower (filter (/= ' ') str)

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys)
    | x < y     = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

-- HC12T9: Read and Print File Content (with error handling)
readFileSafe :: FilePath -> IO ()
readFileSafe filename =
    catchIOError (do
        contents <- readFile filename
        putStrLn ("File content:\n" ++ contents))
        (\_ -> putStrLn "Error: File not found or cannot be read.")

-- HC12T10: Mathematical Operations Module (simple demo)
add :: Int -> Int -> Int
add x y = x + y

subtractNum :: Int -> Int -> Int
subtractNum x y = x - y

multiply :: Int -> Int -> Int
multiply x y = x * y

divide :: Float -> Float -> Float
divide x y = x / y


-- ==========================
-- MAIN PROGRAM
-- ==========================
main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 12 Practical Tasks ==="

    -- HC12T1
    putStrLn "\nHC12T1: Print a Welcome Message"
    putStrLn "Welcome to Haskell Programming!"

    -- HC12T2
    putStrLn "\nHC12T2: Add Two Numbers"
    print (addTwoNumbers 5 7)

    -- HC12T3
    putStrLn "\nHC12T3: Factorial Function"
    print (factorial 5)

    -- HC12T4
    putStrLn "\nHC12T4: First 10 Fibonacci Numbers"
    print [fibonacci n | n <- [0..9]]

    -- HC12T5
    putStrLn "\nHC12T5: Palindrome Checker"
    let word = "Racecar"
    putStrLn (word ++ " -> " ++ show (isPalindrome word))

    -- HC12T6
    putStrLn "\nHC12T6: Sort a List of Integers"
    let nums = [9, 4, 1, 7, 3, 8]
    print (sort nums)

    -- HC12T7
    putStrLn "\nHC12T7: Calculate Circle Area"
    print (calculateCircleArea 5)

    -- HC12T8
    putStrLn "\nHC12T8: Merge Two Sorted Lists"
    print (mergeLists [1,3,5] [2,4,6])

    -- HC12T9
    putStrLn "\nHC12T9: Read and Print File Content (Demo)"
    readFileSafe "sample.txt"  -- Create sample.txt to test

    -- HC12T10
    putStrLn "\nHC12T10: Mathematical Operations Module"
    putStrLn ("Add: " ++ show (add 10 5))
    putStrLn ("Subtract: " ++ show (subtractNum 10 5))
    putStrLn ("Multiply: " ++ show (multiply 10 5))
    putStrLn ("Divide: " ++ show (divide 10 5))

    putStrLn "\n✅ All Tasks Completed Successfully!"
