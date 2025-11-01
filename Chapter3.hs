import Text.Printf (printf)

checkNumber :: Int -> String
checkNumber x =
    if x > 0 then "Positive"
    else if x < 0 then "Negative"
    else "Zero"

grade :: Int -> String
grade x
    | x >= 90 = "A"
    | x >= 80 = "B"
    | x >= 70 = "C"
    | x >= 60 = "D"
    | otherwise = "F"

rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let
        toHex n = printf "%02X" n
        red = toHex r
        green = toHex g
        blue = toHex b
    in "#" ++ red ++ green ++ blue

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2 = "Winter"
    | month == 3 || month == 4 || month == 5 = "Spring"
    | month == 6 || month == 7 || month == 8 = "Summer"
    | month == 9 || month == 10 || month == 11 = "Autumn"
    | otherwise = "Invalid month"

bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25.0 = "Normal"
    | bmi < 30.0 = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / (height ^ 2)

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = max a b
    in max maxAB c

isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False

main :: IO ()
main = do
    putStrLn ("checkNumber 5 = " ++ checkNumber 5)
    putStrLn ("checkNumber (-3) = " ++ checkNumber (-3))
    putStrLn ("checkNumber 0 = " ++ checkNumber 0)

    putStrLn ("\ngrade 95 = " ++ grade 95)
    putStrLn ("grade 72 = " ++ grade 72)
    putStrLn ("grade 50 = " ++ grade 50)

    putStrLn ("\nrgbToHex (255, 0, 127) = " ++ rgbToHex (255, 0, 127))
    putStrLn ("rgbToHex (0, 255, 64) = " ++ rgbToHex (0, 255, 64))

    putStrLn ("\ntriangleArea 3 4 5 = " ++ show (triangleArea 3 4 5))
    putStrLn ("triangleArea 7 8 9 = " ++ show (triangleArea 7 8 9))

    putStrLn ("\ntriangleType 3 3 3 = " ++ triangleType 3 3 3)
    putStrLn ("triangleType 5 5 8 = " ++ triangleType 5 5 8)
    putStrLn ("triangleType 6 7 8 = " ++ triangleType 6 7 8)

    putStrLn ("\nisLeapYear 2000 = " ++ show (isLeapYear 2000))
    putStrLn ("isLeapYear 1900 = " ++ show (isLeapYear 1900))
    putStrLn ("isLeapYear 2024 = " ++ show (isLeapYear 2024))

    putStrLn ("\nseason 3 = " ++ season 3)
    putStrLn ("season 7 = " ++ season 7)
    putStrLn ("season 11 = " ++ season 11)

    putStrLn ("\nbmiCategory 70 1.75 = " ++ bmiCategory 70 1.75)
    putStrLn ("bmiCategory 90 1.8 = " ++ bmiCategory 90 1.8)

    putStrLn ("\nmaxOfThree 10 20 15 = " ++ show (maxOfThree 10 20 15))
    putStrLn ("maxOfThree 5 25 10 = " ++ show (maxOfThree 5 25 10))

    putStrLn ("\nisPalindrome \"racecar\" = " ++ show (isPalindrome "racecar"))
    putStrLn ("isPalindrome \"haskell\" = " ++ show (isPalindrome "haskell"))
    putStrLn ("isPalindrome \"madam\" = " ++ show (isPalindrome "madam"))

