module Main where
import Data.Char (toLower)

---------------------------------------------------------
-- HC18T1: mapToLower Function with fmap
---------------------------------------------------------
mapToLower :: String -> String
mapToLower = fmap toLower

---------------------------------------------------------
-- HC18T2: Functor Instance for Tree
---------------------------------------------------------
data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

---------------------------------------------------------
-- HC18T3: incrementTreeValues Function
---------------------------------------------------------
incrementTreeValues :: Num a => Tree a -> Tree a
incrementTreeValues = fmap (+1)

---------------------------------------------------------
-- HC18T4: mapToBits Function
---------------------------------------------------------
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

---------------------------------------------------------
-- HC18T5: Functor Instance for Either
-- NOTE: Already provided by Prelude/Data.Either, so we skip redefining it.
---------------------------------------------------------
-- instance Functor (Either e) where
--   fmap _ (Left x) = Left x
--   fmap f (Right y) = Right (f y)

---------------------------------------------------------
-- HC18T6: applyToMaybe Function
---------------------------------------------------------
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

---------------------------------------------------------
-- HC18T7: fmapTuple Function
---------------------------------------------------------
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

---------------------------------------------------------
-- HC18T8: identityLawCheck Function
---------------------------------------------------------
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

---------------------------------------------------------
-- HC18T9: compositionLawCheck Function
---------------------------------------------------------
compositionLawCheck :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

---------------------------------------------------------
-- HC18T10: nestedFmap Function
---------------------------------------------------------
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap

---------------------------------------------------------
-- MAIN: Demonstration of All Tasks
---------------------------------------------------------
main :: IO ()
main = do
  putStrLn "==== Haskell Chapter 18: Functors and fmap ===="

  -- HC18T1
  putStrLn "\nHC18T1: mapToLower"
  print (mapToLower "HELLO WORLD")  -- "hello world"

  -- HC18T2 & HC18T3
  let tree = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
  putStrLn "\nHC18T2 & HC18T3: Functor for Tree and incrementTreeValues"
  print tree
  print (incrementTreeValues tree)   -- Each value +1

  -- HC18T4
  putStrLn "\nHC18T4: mapToBits"
  print (mapToBits [True, False, True, True])  -- "1011"

  -- HC18T5
  putStrLn "\nHC18T5: Functor for Either (built-in)"
  print (fmap (+1) (Right 10 :: Either String Int))  -- Right 11
  print (fmap (+1) (Left "Error" :: Either String Int))  -- Left "Error"

  -- HC18T6
  putStrLn "\nHC18T6: applyToMaybe"
  print (applyToMaybe (*2) (Just 5))   -- Just 10
  print (applyToMaybe (*2) Nothing)    -- Nothing

  -- HC18T7
  putStrLn "\nHC18T7: fmapTuple"
  print (fmapTuple length ("Name", "Mogau"))  -- ("Name", 5)

  -- HC18T8
  putStrLn "\nHC18T8: identityLawCheck"
  print (identityLawCheck (Just 7))    -- True
  print (identityLawCheck [1,2,3])     -- True

  -- HC18T9
  putStrLn "\nHC18T9: compositionLawCheck"
  print (compositionLawCheck (+1) (*2) (Just 3))  -- True
  print (compositionLawCheck (++"!") reverse ["hi"])  -- True

  -- HC18T10
  putStrLn "\nHC18T10: nestedFmap"
  print (nestedFmap (+1) [[Just 1, Nothing], [Just 3]])  -- [[Just 2,Nothing],[Just 4]]
