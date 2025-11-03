{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception
import System.IO
import Text.Read (readMaybe)
import Data.Typeable

----------------------------------------------
-- HC15T3: Custom Exception for Traffic Light
----------------------------------------------
data TrafficLightException = InvalidLight String
  deriving (Show, Typeable)

instance Exception TrafficLightException

----------------------------------------------
-- HC15T2: Self-Driving AI Car System
----------------------------------------------
selfDrivingCar :: String -> String
selfDrivingCar "green"  = "Car is moving forward."
selfDrivingCar "yellow" = "Car is slowing down."
selfDrivingCar "red"    = "Car is stopping."
selfDrivingCar light    = throw (InvalidLight ("Unknown traffic light: " ++ light))

----------------------------------------------
-- HC15T4: Exception Handler for Traffic Light
----------------------------------------------
handleTrafficLight :: TrafficLightException -> IO ()
handleTrafficLight (InvalidLight msg) = putStrLn ("Traffic Light Error: " ++ msg)

----------------------------------------------
-- HC15T5: Safe Division Using Maybe
----------------------------------------------
safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv x y = Just (x / y)

----------------------------------------------
-- HC15T6: Safe Input Parsing with readMaybe
----------------------------------------------
safeRead :: Read a => String -> Maybe a
safeRead = readMaybe

----------------------------------------------
-- HC15T8: Division with Either for Detailed Errors
----------------------------------------------
divideEither :: Double -> Double -> Either String Double
divideEither _ 0 = Left "Error: Cannot divide by zero."
divideEither x y = Right (x / y)

----------------------------------------------
-- HC15T9: Try Function for File IO Exceptions
----------------------------------------------
readFileSafe :: FilePath -> IO ()
readFileSafe file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left err -> putStrLn ("File error: " ++ show err)
    Right content -> putStrLn ("File content:\n" ++ content)

----------------------------------------------
-- HC15T7: Velocity Calculation with Optional Parsing
----------------------------------------------
calcVelocityMaybe :: Maybe Double -> Maybe Double -> Maybe Double
calcVelocityMaybe (Just dist) (Just time)
  | time /= 0  = Just (dist / time)
  | otherwise  = Nothing
calcVelocityMaybe _ _ = Nothing

----------------------------------------------
-- HC15T10: Hybrid Error Handling (Either + IO)
----------------------------------------------
calcVelocityHybrid :: String -> String -> IO ()
calcVelocityHybrid distStr timeStr = do
  let maybeDist = readMaybe distStr :: Maybe Double
      maybeTime = readMaybe timeStr :: Maybe Double
  case (maybeDist, maybeTime) of
    (Just d, Just t) -> case divideEither d t of
        Left err -> putStrLn err
        Right v  -> putStrLn ("Velocity: " ++ show v ++ " m/s")
    _ -> putStrLn "Error: Invalid numeric input!"

----------------------------------------------
-- HC15T1: Handle Exceptions for File Reading and Velocity
----------------------------------------------
velocityFromFile :: FilePath -> IO ()
velocityFromFile file = do
  result <- try (readFile file) :: IO (Either IOException String)
  case result of
    Left err -> putStrLn ("File error: " ++ show err)
    Right content -> do
      let linesOfFile = words content
      case linesOfFile of
        [dStr, tStr] -> calcVelocityHybrid dStr tStr
        _ -> putStrLn "File format error! Expecting two numbers: distance and time."

----------------------------------------------
-- MAIN (Demonstration for all tasks)
----------------------------------------------
main :: IO ()
main = do
  putStrLn "==== Haskell Chapter 15: Exception and Error Handling ===="

  putStrLn "\nHC15T2–HC15T4: Self-driving car and exception handling"
  catch (print (selfDrivingCar "green")) handleTrafficLight
  catch (print (selfDrivingCar "purple")) handleTrafficLight

  putStrLn "\nHC15T5: Safe Division Using Maybe"
  print (safeDiv 10 2)
  print (safeDiv 10 0)

  putStrLn "\nHC15T6: Safe Input Parsing"
  print (safeRead "42" :: Maybe Int)
  print (safeRead "abc" :: Maybe Int)

  putStrLn "\nHC15T7: Velocity Calculation with Maybe"
  print (calcVelocityMaybe (Just 100) (Just 10))
  print (calcVelocityMaybe (Just 50) (Just 0))
  print (calcVelocityMaybe Nothing (Just 5))

  putStrLn "\nHC15T8: Division with Either"
  print (divideEither 10 2)
  print (divideEither 10 0)

  putStrLn "\nHC15T9: Try Function for File IO"
  readFileSafe "nonexistent.txt"

  putStrLn "\nHC15T10: Hybrid Error Handling"
  calcVelocityHybrid "100" "5"
  calcVelocityHybrid "100" "0"
  calcVelocityHybrid "abc" "5"

  putStrLn "\nHC15T1: File reading and velocity (example file: data.txt)"
  putStrLn "(If file not found, it will handle gracefully.)"
  velocityFromFile "data.txt"
