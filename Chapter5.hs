module Main where
import Data.Char (isUpper)

applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

filterOddNumbers :: [Int]
filterOddNumbers = filter odd [1..30]

hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && isUpper (head word))

biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

multiplyByFive :: Int -> Int
multiplyByFive = (*5)

evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)

result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

addFive :: Int -> Int
addFive = (+5)

transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2)

main :: IO ()
main = do
  print (applyThrice (+1) 2)
  print filterOddNumbers
  print (hasUppercaseWord ["apple", "Banana", "cherry"])
  print (biggerThan10 15)
  print (multiplyByFive 8)
  print (evenSquares [1..10])
  print result
  print (addFive 12)
  print (transformList (*2) [1,2,3])
  print (anySquareGreaterThan50 [3,5,8])
