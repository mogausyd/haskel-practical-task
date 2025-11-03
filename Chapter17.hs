module Main where
-- We hide Min, Max, Sum, Product from Data.Semigroup to avoid clashes
import Data.Semigroup hiding (Min, Max, Sum, Product)

---------------------------------------------------------
-- HC17T1: Severity Data Type and Semigroup Instance
---------------------------------------------------------
data Severity = Low | Medium | High | Critical
  deriving (Show, Eq, Ord)

instance Semigroup Severity where
  (<>) = max  -- higher severity overrides lower one

---------------------------------------------------------
-- HC17T2: Min and Max Newtypes with Semigroup
---------------------------------------------------------
newtype Min a = Min { getMin :: a } deriving (Show, Eq, Ord)
newtype Max a = Max { getMax :: a } deriving (Show, Eq, Ord)

instance (Ord a) => Semigroup (Min a) where
  (Min a) <> (Min b) = Min (min a b)

instance (Ord a) => Semigroup (Max a) where
  (Max a) <> (Max b) = Max (max a b)

---------------------------------------------------------
-- HC17T3: Monoid Instance for Severity
---------------------------------------------------------
instance Monoid Severity where
  mempty = Low

---------------------------------------------------------
-- HC17T4: Monoid Instance for Sum Newtype
---------------------------------------------------------
newtype Sum a = Sum { getSum :: a } deriving (Show, Eq, Ord)

instance (Num a) => Semigroup (Sum a) where
  (Sum a) <> (Sum b) = Sum (a + b)

instance (Num a) => Monoid (Sum a) where
  mempty = Sum 0

---------------------------------------------------------
-- HC17T5: combineLists Function
---------------------------------------------------------
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

---------------------------------------------------------
-- HC17T6: maxSeverity Function
---------------------------------------------------------
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

---------------------------------------------------------
-- HC17T7: multiplyProducts Function
---------------------------------------------------------
newtype Product a = Product { getProduct :: a } deriving (Show, Eq, Ord)

instance (Num a) => Semigroup (Product a) where
  (Product a) <> (Product b) = Product (a * b)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1

multiplyProducts :: (Num a) => [Product a] -> Product a
multiplyProducts = mconcat

---------------------------------------------------------
-- HC17T8: foldWithSemigroup Function
---------------------------------------------------------
foldWithSemigroup :: (Semigroup a) => [a] -> a
foldWithSemigroup = foldr1 (<>)

---------------------------------------------------------
-- HC17T9: Config Data Type and Semigroup Instance
---------------------------------------------------------
data Config = Config
  { loggingLevel :: Severity
  , timeout      :: Int
  , retries      :: Int
  } deriving (Show, Eq)

instance Semigroup Config where
  c1 <> c2 = Config
    { loggingLevel = max (loggingLevel c1) (loggingLevel c2)
    , timeout      = min (timeout c1) (timeout c2)
    , retries      = max (retries c1) (retries c2)
    }

---------------------------------------------------------
-- HC17T10: Monoid Instance for Config
---------------------------------------------------------
instance Monoid Config where
  mempty = Config { loggingLevel = Low, timeout = maxBound, retries = 0 }

---------------------------------------------------------
-- MAIN: Demonstration of All Tasks
---------------------------------------------------------
main :: IO ()
main = do
  putStrLn "==== Haskell Chapter 17: Semigroups and Monoids ===="

  putStrLn "\nHC17T1: Severity Combination"
  print (Medium <> High)       -- High
  print (Low <> Critical)      -- Critical

  putStrLn "\nHC17T2: Min and Max Semigroup"
  print ((Min 5) <> (Min 3))   -- Min 3
  print ((Max 5) <> (Max 3))   -- Max 5

  putStrLn "\nHC17T3: Monoid Severity"
  print (mempty <> Medium)     -- Medium

  putStrLn "\nHC17T4: Sum Monoid"
  print (Sum 5 <> Sum 10)      -- Sum 15
  print (mconcat [Sum 1, Sum 2, Sum 3])  -- Sum 6

  putStrLn "\nHC17T5: combineLists"
  print (combineLists [1,2,3] [4,5])     -- [1,2,3,4,5]

  putStrLn "\nHC17T6: maxSeverity"
  print (maxSeverity [Low, High, Medium])  -- High

  putStrLn "\nHC17T7: multiplyProducts"
  print (multiplyProducts [Product 2, Product 3, Product 4])  -- Product 24

  putStrLn "\nHC17T8: foldWithSemigroup"
  print (foldWithSemigroup [Sum 2, Sum 3, Sum 5])             -- Sum 10

  putStrLn "\nHC17T9 & HC17T10: Config Combination"
  let config1 = Config Medium 50 2
  let config2 = Config High 30 5
  print (config1 <> config2)
  print (mempty <> config1)
