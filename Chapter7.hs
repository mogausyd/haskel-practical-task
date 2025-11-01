module Main where

data Color = Red | Green | Blue deriving (Show, Read, Enum, Bounded)

instance Eq Color where
  Red == Red = True
  Green == Green = True
  Blue == Blue = True
  _ == _ = False

instance Ord Color where
  compare Red Red = EQ
  compare Red _ = LT
  compare Green Red = GT
  compare Green Green = EQ
  compare Green Blue = LT
  compare Blue Blue = EQ
  compare Blue _ = GT

compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

data Shape = Circle Double | Rectangle Double Double deriving (Eq, Read, Show)

squareArea :: Num a => a -> a
squareArea side = side * side

circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

nextColor :: Color -> Color
nextColor c
  | c == maxBound = minBound
  | otherwise = succ c

parseShape :: String -> Maybe Shape
parseShape s = case reads s :: [(Shape, String)] of
  [(val, "")] -> Just val
  _ -> Nothing

class Describable a where
  describe :: a -> String

instance Describable Bool where
  describe True = "This is True."
  describe False = "This is False."

instance Describable Shape where
  describe (Circle r) = "A circle with radius " ++ show r
  describe (Rectangle w h) = "A rectangle of width " ++ show w ++ " and height " ++ show h

describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
  | x >= y = describe x
  | otherwise = describe y

main :: IO ()
main = do
  print (Red == Green)
  print (compareValues 10 20)
  print (squareArea 5)
  print (circleCircumference 7)
  print (nextColor Red)
  print (parseShape "Circle 4.5")
  print (describe (Rectangle 3 4))
