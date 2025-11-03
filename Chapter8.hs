module Main where

-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr amount =
  "From: " ++ fromAddr ++ " | To: " ++ toAddr ++ " | Value: " ++ show amount

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

data Person = PersonData {
  personName :: String,
  personAddress :: (String, Int),
  paymentMethod :: PaymentMethod
} deriving (Show)

bob :: Person
bob = PersonData "Bob" ("123 Main St", 1234) Cash

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4: Record Syntax for Employee
data Employee = Employee {
  name :: String,
  experienceInYears :: Float
} deriving (Show)

richard :: Employee
richard = Employee "Richard" 7.5

-- HC8T5: Record Syntax for Person
data PersonRecord = PersonRecord {
  pname :: String,
  age :: Int,
  isEmployed :: Bool
} deriving (Show)

person1 :: PersonRecord
person1 = PersonRecord "Alice" 30 True

person2 :: PersonRecord
person2 = PersonRecord "John" 25 False

-- HC8T6: Record Syntax for Shape Variants
data ShapeRecord
  = CircleRecord { center :: (Float, Float), color :: String, radius :: Float }
  | RectangleRecord { width :: Float, height :: Float, color :: String }
  deriving (Show)

circle1 :: ShapeRecord
circle1 = CircleRecord (0.0, 0.0) "Red" 5.0

rectangle1 :: ShapeRecord
rectangle1 = RectangleRecord 10.0 5.0 "Blue"

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog name) = name ++ " is a loyal dog."
describeAnimal (Cat name) = name ++ " is a playful cat."

dog1 :: Animal
dog1 = Dog "Buddy"

cat1 :: Animal
cat1 = Cat "Mittens"

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction {
  from :: Address,
  to :: Address,
  amount :: Value,
  transactionId :: String
} deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
  let txId = "TX-" ++ show (length fromAddr + length toAddr + amt)
      tx = Transaction fromAddr toAddr amt txId
  in transactionId tx

-- HC8T10: Deriving Show for Book
data Book = Book {
  title :: String,
  author :: String,
  year :: Int
} deriving (Show)

myBook :: Book
myBook = Book "Learn You a Haskell" "Miran Lipovača" 2011

-- MAIN to test all
main :: IO ()
main = do
  putStrLn (generateTx "Alice" "Bob" 100)
  print bob
  print (area (Circle 5))
  print (area (Rectangle 10 5))
  print richard
  print person1
  print person2
  print circle1
  print rectangle1
  putStrLn (describeAnimal dog1)
  putStrLn (describeAnimal cat1)
  putStrLn (greet "Mogau" 21)
  putStrLn ("Transaction ID: " ++ createTransaction "Addr1" "Addr2" 50)
  print myBook
