-- HC10T1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show, Eq)

class ShowSimple a where
    showSimple :: a -> String

instance ShowSimple PaymentMethod where
    showSimple Cash = "Cash Payment"
    showSimple Card = "Card Payment"
    showSimple Cryptocurrency = "Cryptocurrency Payment"


-- HC10T2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp = sum


-- HC10T3: Comparable Type Class
data Blockchain = Blockchain { chainId :: Int, chainName :: String } deriving (Show)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith b1 b2 = compare (chainId b1) (chainId b2)


-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance (Eq a) => Eq (Box a) where
    Empty == Empty = True
    (Has x) == (Has y) = x == y
    _ == _ = False


-- HC10T5: ShowDetailed Type Class
data User = User { username :: String, age :: Int } deriving (Show)

class ShowDetailed a where
    showDetailed :: a -> String

instance ShowDetailed User where
    showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age


-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
    (==) b1 b2 = chainId b1 == chainId b2 && chainName b1 == chainName b2
    (/=) b1 b2 = not (b1 == b2)


-- HC10T7: Convertible Type Class
class Convertible a b where
    convert :: a -> b

instance Convertible PaymentMethod String where
    convert Cash = "Cash"
    convert Card = "Card"
    convert Cryptocurrency = "Cryptocurrency"


-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
    compareEquality x y = x == y


-- HC10T9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a

instance MinMax Int where
    minValue = minBound
    maxValue = maxBound


-- HC10T10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable String where
    concatWith a b = a ++ b


-- Example Testing
main :: IO ()
main = do
    -- HC10T1
    print (showSimple Cash)
    print (showSimple Card)

    -- HC10T2
    print (sumUp [1, 2, 3, 4 :: Int])

    -- HC10T3
    let b1 = Blockchain 1 "Alpha"
    let b2 = Blockchain 2 "Beta"
    print (compareWith b1 b2)

    -- HC10T4
    print (Has 5 == Has 5)
    print (Empty == Has 1)

    -- HC10T5
    let u1 = User "Alice" 25
    putStrLn (showDetailed u1)

    -- HC10T6
    print (b1 == b2)
    print (b1 /= b2)

    -- HC10T7
    print (convert Cryptocurrency :: String)

    -- HC10T8
    print (compareEquality (5 :: Int) 5)
    print (compareEquality (5 :: Int) 7)

    -- HC10T9
    print (minValue :: Int)
    print (maxValue :: Int)

    -- HC10T10
    print (concatWith "Hello " "World")
