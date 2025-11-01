module Main where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

sumUsingFoldr :: [Int] -> Int
sumUsingFoldr = foldr (+) 0

productUsingFoldl :: [Int] -> Int
productUsingFoldl = foldl (*) 1

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
  | e == x = True
  | otherwise = elementExists e xs

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
  | even x = x : filterEven xs
  | otherwise = filterEven xs

mapFunction :: (a -> b) -> [a] -> [b]
mapFunction _ [] = []
mapFunction f (x:xs) = f x : mapFunction f xs

digitsOfNumber :: Int -> [Int]
digitsOfNumber n
  | n < 10 = [n]
  | otherwise = digitsOfNumber (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = do
  print (factorial 5)
  print (fibonacci 8)
  print (sumUsingFoldr [1,2,3,4,5])
  print (productUsingFoldl [1,2,3,4,5])
  print (reverseList [1,2,3,4,5])
  print (elementExists 3 [1,2,3,4,5])
  print (listLength [10,20,30,40])
  print (filterEven [1..10])
  print (mapFunction (*2) [1,2,3,4])
  print (digitsOfNumber 12345)
