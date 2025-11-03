module Main where

import Control.Applicative
import Control.Monad (forever, when, replicateM)

---------------------------------------------------------
-- HC19T1: Applicative Instance for Pair
---------------------------------------------------------
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

---------------------------------------------------------
-- HC19T2: addThreeApplicative Function
---------------------------------------------------------
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

---------------------------------------------------------
-- HC19T3: safeProduct for Maybe Int
---------------------------------------------------------
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = fmap product . sequenceA

---------------------------------------------------------
-- HC19T4: liftAndMultiply with liftA2
---------------------------------------------------------
liftAndMultiply :: Int -> Int -> Int
liftAndMultiply = (*)

liftAndMultiplyApp :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiplyApp = liftA2 (*)

---------------------------------------------------------
-- HC19T5: applyEffects with <*>
---------------------------------------------------------
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a, b) = (+) <$> a <*> b

---------------------------------------------------------
-- HC19T6: repeatEffect with forever
---------------------------------------------------------
repeatEffect :: IO () -> IO ()
repeatEffect = forever

---------------------------------------------------------
-- HC19T7: conditionalPrint with when
---------------------------------------------------------
conditionalPrint :: Bool -> String -> IO ()
conditionalPrint cond msg = when cond (putStrLn msg)

---------------------------------------------------------
-- HC19T8: discardSecond with <*
---------------------------------------------------------
discardSecond :: Applicative f => f a -> f b -> f a
discardSecond = (<*)

---------------------------------------------------------
-- HC19T9: pureAndApply Demonstration
---------------------------------------------------------
pureAndApply :: IO ()
pureAndApply = do
  let f = pure (+3) <*> pure 7 :: Maybe Int
  print f  -- Just 10

---------------------------------------------------------
-- HC19T10: combineResults for Either
---------------------------------------------------------
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

---------------------------------------------------------
-- HC19T11: Applicative Instance for Wrapper
---------------------------------------------------------
data Wrapper a = Wrapper a deriving (Show, Eq)

instance Functor Wrapper where
  fmap f (Wrapper x) = Wrapper (f x)

instance Applicative Wrapper where
  pure = Wrapper
  (Wrapper f) <*> (Wrapper x) = Wrapper (f x)

---------------------------------------------------------
-- HC19T12: sumThreeApplicative for Either String Int
---------------------------------------------------------
sumThreeApplicative :: Either String Int -> Either String Int -> Either String Int -> Either String Int
sumThreeApplicative = liftA3 (\x y z -> x + y + z)

---------------------------------------------------------
-- HC19T13: whenApplicative Function
---------------------------------------------------------
whenApplicative :: Bool -> IO () -> IO ()
whenApplicative = when

---------------------------------------------------------
-- HC19T14: replicateEffect with replicateM
---------------------------------------------------------
replicateEffect :: Int -> IO a -> IO [a]
replicateEffect = replicateM

---------------------------------------------------------
-- HC19T15: sequenceEffects for Applicative List
---------------------------------------------------------
sequenceEffects :: Applicative f => [f a] -> f [a]
sequenceEffects = sequenceA

---------------------------------------------------------
-- HC19T16: applyWithEffects and <*>
---------------------------------------------------------
applyWithEffects :: Applicative f => f (a -> b) -> f a -> f b
applyWithEffects = (<*>)

---------------------------------------------------------
-- HC19T17: simulateMaybeEffect for Multiple Maybe
---------------------------------------------------------
simulateMaybeEffect :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
simulateMaybeEffect = liftA3 (\x y z -> x * y + z)

---------------------------------------------------------
-- HC19T18: combineEitherResults with Multiple Either
---------------------------------------------------------
combineEitherResults :: Either String Int -> Either String Int -> Either String Int -> Either String Int
combineEitherResults = liftA3 (\x y z -> x + y + z)

---------------------------------------------------------
-- HC19T19: sequenceApplicative for Maybe List
---------------------------------------------------------
sequenceApplicative :: [Maybe a] -> Maybe [a]
sequenceApplicative = sequenceA

---------------------------------------------------------
-- HC19T20: replicateForever with forever
---------------------------------------------------------
replicateForever :: IO () -> IO ()
replicateForever = forever

---------------------------------------------------------
-- MAIN: Demonstration of All Tasks
---------------------------------------------------------
main :: IO ()
main = do
  putStrLn "==== Haskell Chapter 19: Applicative Functors and Effects ===="

  -- HC19T1
  putStrLn "\nHC19T1: Pair Applicative"
  print (Pair (+1) (*2) <*> Pair 5 10)  -- Pair 6 20

  -- HC19T2
  putStrLn "\nHC19T2: addThreeApplicative"
  print (addThreeApplicative (Just 1) (Just 2) (Just 3))  -- Just 6

  -- HC19T3
  putStrLn "\nHC19T3: safeProduct"
  print (safeProduct [Just 2, Just 3, Just 4])   -- Just 24
  print (safeProduct [Just 2, Nothing, Just 4])  -- Nothing

  -- HC19T4
  putStrLn "\nHC19T4: liftAndMultiplyApp"
  print (liftAndMultiplyApp (Just 3) (Just 5))   -- Just 15

  -- HC19T5
  putStrLn "\nHC19T5: applyEffects"
  result <- applyEffects (pure 4, pure 6)
  print result  -- 10

  -- HC19T6 & HC19T7
  putStrLn "\nHC19T7: conditionalPrint"
  conditionalPrint True "Condition met!"
  conditionalPrint False "This will not print."

  -- HC19T8
  putStrLn "\nHC19T8: discardSecond"
  print (Just "Hello" <* Just "World")  -- Just "Hello"

  -- HC19T9
  putStrLn "\nHC19T9: pureAndApply"
  pureAndApply

  -- HC19T10
  putStrLn "\nHC19T10: combineResults"
  print (combineResults (Right 5) (Right 10))  -- Right 15
  print (combineResults (Left "Error") (Right 10))  -- Left "Error"

  -- HC19T11
  putStrLn "\nHC19T11: Wrapper Applicative"
  print (Wrapper (+2) <*> Wrapper 8)  -- Wrapper 10

  -- HC19T12
  putStrLn "\nHC19T12: sumThreeApplicative"
  print (sumThreeApplicative (Right 2) (Right 3) (Right 4))  -- Right 9

  -- HC19T13
  putStrLn "\nHC19T13: whenApplicative"
  whenApplicative True (putStrLn "Runs because True")

  -- HC19T14
  putStrLn "\nHC19T14: replicateEffect"
  xs <- replicateEffect 3 (pure 5)
  print xs  -- [5,5,5]

  -- HC19T15
  putStrLn "\nHC19T15: sequenceEffects"
  print (sequenceEffects [Just 1, Just 2, Just 3])  -- Just [1,2,3]

  -- HC19T16
  putStrLn "\nHC19T16: applyWithEffects"
  print (applyWithEffects (Just (+5)) (Just 10))  -- Just 15

  -- HC19T17
  putStrLn "\nHC19T17: simulateMaybeEffect"
  print (simulateMaybeEffect (Just 2) (Just 3) (Just 4))  -- Just 10

  -- HC19T18
  putStrLn "\nHC19T18: combineEitherResults"
  print (combineEitherResults (Right 1) (Right 2) (Right 3))  -- Right 6

  -- HC19T19
  putStrLn "\nHC19T19: sequenceApplicative"
  print (sequenceApplicative [Just 10, Just 20, Just 30])  -- Just [10,20,30]

  -- HC19T20
  putStrLn "\nHC19T20: replicateForever (commented out to prevent infinite loop)"
  putStrLn "replicateForever (putStrLn \"Hi\") -- would run forever"
