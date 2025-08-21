import Data.List (sortBy)

-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected types:
-- 42              :: Int
-- 3.14            :: Double
-- "Haskell"       :: String
-- 'Z'             :: Char
-- True && False   :: Bool

-- HC2T2 - Task 2: Function Type Signatures
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3 - Task 3: Immutable Variables
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4 - Task 4: Infix and Prefix Examples
prefix1 = (+) 5 3       -- 5 + 3
prefix2 = (*) 10 4      -- 10 * 4
prefix3 = (&&) True False -- True && False

infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

-- HC2T5 - Task 5: Defining and Using Functions
circleArea :: Float -> Float
circleArea r = pi * r^2

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6 - Task 6: Int vs Integer
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- HC2T7 - Task 7: Boolean Expressions
expr1 = (5 > 3) && (10 > 2)   -- True
expr2 = False || False        -- False
expr3 = not False             -- True
expr4 = 7 > 10                -- False

-- Main function to demonstrate all tasks
main :: IO ()
main = do
    putStrLn "HC2T1: Checking types (manually expected, not runtime)"
    putStrLn "42 :: Int, 3.14 :: Double, \"Haskell\" :: String, 'Z' :: Char, True && False :: Bool"

    putStrLn "\nHC2T2: Function tests"
    print (add 3 5)                     -- 8
    print (isEven 4)                    -- True
    print (concatStrings "Hello, " "World!")

    putStrLn "\nHC2T3: Immutable variables"
    print myAge
    print piValue
    print greeting
    print isHaskellFun

    putStrLn "\nHC2T4: Infix and Prefix conversions"
    print prefix1
    print prefix2
    print prefix3
    print infix1
    print infix2
    print infix3

    putStrLn "\nHC2T5: Custom functions"
    print (circleArea 5)
    print (maxOfThree 3 7 5)

    putStrLn "\nHC2T6: Int vs Integer"
    print smallNumber
    print bigNumber
    putStrLn "Try evaluating (2^64 :: Int) in GHCi to see overflow"
    putStrLn "Try evaluating (2^64 :: Integer) in GHCi for correct value"

    putStrLn "\nHC2T7: Boolean expressions"
    print expr1
    print expr2
    print expr3
    print expr4
