module Main where

weatherReport :: String -> String
weatherReport "sunny" = "It's a bright and beautiful day!"
weatherReport "rainy" = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _ = "Weather unknown"

dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday" = "It's a weekend!"
dayType "Monday" = "It's a weekday."
dayType "Tuesday" = "It's a weekday."
dayType "Wednesday" = "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday" = "It's a weekday."
dayType _ = "Invalid day"

gradeComment :: Int -> String
gradeComment n
  | n >= 90 && n <= 100 = "Excellent!"
  | n >= 70 && n <= 89 = "Good job!"
  | n >= 50 && n <= 69 = "You passed."
  | n >= 0 && n <= 49 = "Better luck next time."
  | otherwise = "Invalid grade"

specialBirthday :: Int -> String
specialBirthday 1 = "First birthdays are always special!"
specialBirthday 18 = "Finally an adult!"
specialBirthday 21 = "Big 21! Celebrate!"
specialBirthday _ = "Happy birthday!"

specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1 = "First birthdays are always special!"
specialBirthdayWithAge 18 = "Finally an adult!"
specialBirthdayWithAge 21 = "Big 21! Celebrate!"
specialBirthdayWithAge age = "Happy " ++ show age ++ "th birthday!"

whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "The list is empty."
whatsInsideThisList [x] = "The list has one element."
whatsInsideThisList [x, y] = "The list has two elements."
whatsInsideThisList _ = "The list has many elements."

firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _ = []

describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

main :: IO ()
main = do
  putStrLn (weatherReport "sunny")
  putStrLn (dayType "Saturday")
  putStrLn (gradeComment 85)
  putStrLn (specialBirthday 21)
  putStrLn (specialBirthdayWithAge 25)
  putStrLn (whatsInsideThisList [1,2,3])
  print (firstAndThird [10,20,30,40])
  putStrLn (describeTuple ("Alice", 30))
