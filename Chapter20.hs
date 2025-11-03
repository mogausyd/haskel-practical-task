{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import Control.Exception (try, SomeException)
import Data.Char (isDigit, isUpper, isLower, isSpace, isAlpha)
import Data.Maybe (maybeToList, fromMaybe)
import Data.List (foldl')

--------------------------------------------------------------------------------
-- HC20T1: Division with Maybe - Alternative approach
--------------------------------------------------------------------------------

divSafe :: (Eq a, Fractional a) => a -> a -> Maybe a
divSafe numerator 0 = Nothing
divSafe numerator denominator = Just (numerator / denominator)

-- Chain multiple divisions
divChain :: (Eq a, Fractional a) => a -> [a] -> Maybe a
divChain start divs = foldM divSafe start divs

--------------------------------------------------------------------------------
-- HC20T2: Sequence Maybe - Alternative implementation
--------------------------------------------------------------------------------

allJust :: [Maybe a] -> Maybe [a]
allJust [] = Just []
allJust (Nothing:_) = Nothing
allJust (Just x:xs) = fmap (x:) (allJust xs)

-- Alternative using traverse
allJustTraverse :: [Maybe a] -> Maybe [a]
allJustTraverse = traverse id

--------------------------------------------------------------------------------
-- HC20T3: Calculator with Writer - Extended version
--------------------------------------------------------------------------------

type LogMsg = [String]
type Calculator a = Writer LogMsg a

addLogged :: Double -> Double -> Calculator Double
addLogged a b = do
  tell ["Adding: " ++ show a ++ " + " ++ show b ++ " = " ++ show result]
  return result
  where result = a + b

subLogged :: Double -> Double -> Calculator Double
subLogged a b = do
  tell ["Subtracting: " ++ show a ++ " - " ++ show b ++ " = " ++ show result]
  return result
  where result = a - b

mulLogged :: Double -> Double -> Calculator Double
mulLogged a b = do
  tell ["Multiplying: " ++ show a ++ " * " ++ show b ++ " = " ++ show result]
  return result
  where result = a * b

divLogged :: Double -> Double -> Calculator (Maybe Double)
divLogged a 0 = do
  tell ["ERROR: Division by zero attempted: " ++ show a ++ " / 0"]
  return Nothing
divLogged a b = do
  tell ["Dividing: " ++ show a ++ " / " ++ show b ++ " = " ++ show result]
  return (Just result)
  where result = a / b

computeExpression :: Double -> Double -> Double -> Double -> Calculator (Maybe Double)
computeExpression w x y z = do
  sum1 <- addLogged w x
  prod <- mulLogged sum1 y
  divLogged prod z

--------------------------------------------------------------------------------
-- HC20T4: Character counting with State - Extended version
--------------------------------------------------------------------------------

countOccurrences :: Char -> String -> Int
countOccurrences target text = execState (mapM_ checkChar text) 0
  where
    checkChar ch = when (ch == target) (modify (+ 1))

-- Count multiple characters at once
countMultiple :: [Char] -> String -> M.Map Char Int
countMultiple targets text = execState (mapM_ processChar text) initialMap
  where
    initialMap = M.fromList [(c, 0) | c <- targets]
    processChar ch = when (ch `elem` targets) $
      modify (M.adjust (+ 1) ch)

--------------------------------------------------------------------------------
-- HC20T5: Reader for Configuration - Enhanced version
--------------------------------------------------------------------------------

data AppConfig = AppConfig
  { greeting :: String
  , useExclamation :: Bool
  , suffix :: String
  }

formatGreeting :: String -> Reader AppConfig String
formatGreeting username = do
  greet <- asks greeting
  exclaim <- asks useExclamation
  suff <- asks suffix
  let punctuation = if exclaim then "!" else "."
  return $ greet ++ " " ++ username ++ punctuation ++ " " ++ suff

standardConfig :: AppConfig
standardConfig = AppConfig "Hi" False "Welcome aboard"

formalConfig :: AppConfig
formalConfig = AppConfig "Good day" True "It's a pleasure"

--------------------------------------------------------------------------------
-- HC20T6: Combining monads - Alternative approach
--------------------------------------------------------------------------------

pairMonads :: Maybe a -> [b] -> [(a, b)]
pairMonads Nothing _ = []
pairMonads (Just x) ys = [(x, y) | y <- ys]

-- More general version
combineMonads :: (Monad m, Monad n) => m a -> n b -> m (n (a, b))
combineMonads ma nb = do
  a <- ma
  return $ do
    b <- nb
    return (a, b)

--------------------------------------------------------------------------------
-- HC20T7: Search with Either - Enhanced version
--------------------------------------------------------------------------------

findElement :: (a -> Bool) -> [a] -> Either String a
findElement predicate items =
  case dropWhile (not . predicate) items of
    (match:_) -> Right match
    [] -> Left "Element not found matching predicate"

-- Find with custom error message
findWithMsg :: (a -> Bool) -> [a] -> String -> Either String a
findWithMsg predicate items errMsg =
  case filter predicate items of
    (x:_) -> Right x
    [] -> Left errMsg

--------------------------------------------------------------------------------
-- HC20T8: Simple Expression Parser - Restructured
--------------------------------------------------------------------------------

newtype SimpleParser a = SP { parse :: String -> Maybe (a, String) }

instance Functor SimpleParser where
  fmap fn (SP parser) = SP $ \input -> do
    (val, remaining) <- parser input
    return (fn val, remaining)

instance Applicative SimpleParser where
  pure val = SP $ \input -> Just (val, input)
  SP pf <*> SP pv = SP $ \input -> do
    (fn, rest1) <- pf input
    (val, rest2) <- pv rest1
    return (fn val, rest2)

instance Monad SimpleParser where
  SP p >>= fn = SP $ \input -> do
    (val, rest) <- p input
    parse (fn val) rest

instance Alternative SimpleParser where
  empty = SP (const Nothing)
  SP p1 <|> SP p2 = SP $ \input -> p1 input <|> p2 input

anyChar :: SimpleParser Char
anyChar = SP $ \input -> case input of
  (c:rest) -> Just (c, rest)
  [] -> Nothing

charIf :: (Char -> Bool) -> SimpleParser Char
charIf predicate = do
  c <- anyChar
  guard (predicate c)
  return c

exactChar :: Char -> SimpleParser Char
exactChar ch = charIf (== ch)

whitespace :: SimpleParser ()
whitespace = void $ many (charIf isSpace)

withToken :: SimpleParser a -> SimpleParser a
withToken p = whitespace *> p <* whitespace

numDigit :: SimpleParser Char
numDigit = charIf isDigit

parseInteger :: SimpleParser Int
parseInteger = withToken $ do
  neg <- optional (exactChar '-')
  digits <- some numDigit
  let num = read digits
  return $ maybe num (const (-num)) neg

inParens :: SimpleParser a -> SimpleParser a
inParens p = withToken (exactChar '(') *> p <* withToken (exactChar ')')

leftAssoc :: SimpleParser a -> SimpleParser (a -> a -> a) -> SimpleParser a
leftAssoc operand operator = do
  first <- operand
  rest first
  where
    rest acc = (do
      op <- operator
      next <- operand
      rest (op acc next)) <|> return acc

plusOp, timesOp :: SimpleParser (Int -> Int -> Int)
plusOp = withToken (exactChar '+') *> return (+)
timesOp = withToken (exactChar '*') *> return (*)

baseFactor :: SimpleParser Int
baseFactor = parseInteger <|> inParens fullExpr

termExpr :: SimpleParser Int
termExpr = baseFactor `leftAssoc` timesOp

fullExpr :: SimpleParser Int
fullExpr = termExpr `leftAssoc` plusOp

evaluateExpr :: String -> Maybe Int
evaluateExpr input = do
  (result, remaining) <- parse (whitespace *> fullExpr <* whitespace) input
  guard (null remaining)
  return result

--------------------------------------------------------------------------------
-- HC20T9: Identity Monad - Alternative implementation
--------------------------------------------------------------------------------

duplicateM :: Int -> a -> Identity [a]
duplicateM count val = Identity (replicate count val)

-- Using replicateM
duplicateWithReplicateM :: Int -> a -> Identity [a]
duplicateWithReplicateM n x = replicateM n (Identity x)

--------------------------------------------------------------------------------
-- HC20T10: Monad Transformers - Stack operations
--------------------------------------------------------------------------------

type Stack a = StateT [a] (MaybeT Identity)

nothingT :: MaybeT Identity b
nothingT = MaybeT (Identity Nothing)

popElement :: Stack a a
popElement = do
  stack <- get
  case stack of
    [] -> lift nothingT
    (top:rest) -> do
      put rest
      return top

popMultiple :: Int -> Stack a [a]
popMultiple 0 = return []
popMultiple n = do
  x <- popElement
  xs <- popMultiple (n - 1)
  return (x : xs)

pushElement :: a -> Stack a ()
pushElement x = modify (x :)

executeStack :: Stack a b -> [a] -> Maybe (b, [a])
executeStack operation initialStack =
  case runIdentity (runMaybeT (runStateT operation initialStack)) of
    Nothing -> Nothing
    Just pair -> Just pair

--------------------------------------------------------------------------------
-- HC20T11: Deterministic Walk - Alternative pattern
--------------------------------------------------------------------------------

type Position = (Int, Int)

movements :: [Position]
movements = [(1,0), (0,1), (-1,0), (0,-1)]

simulateWalk :: Int -> [Position]
simulateWalk steps = scanl move (0,0) (take steps (cycle movements))
  where
    move (x,y) (dx,dy) = (x+dx, y+dy)

--------------------------------------------------------------------------------
-- HC20T12: File I/O - Enhanced version
--------------------------------------------------------------------------------

displayFileContent :: FilePath -> IO ()
displayFileContent path = do
  content <- readFile path
  let numbered = zip [1 :: Int ..] (lines content)
  forM_ numbered $ \(lineNum, lineText) ->
    putStrLn $ "[" ++ show lineNum ++ "] " ++ lineText

--------------------------------------------------------------------------------
-- HC20T13: Memoized Fibonacci - Alternative implementation
--------------------------------------------------------------------------------

fibMemo :: Int -> State (M.Map Int Integer) Integer
fibMemo n
  | n < 2 = return (toInteger n)
  | otherwise = do
      cache <- get
      case M.lookup n cache of
        Just val -> return val
        Nothing -> do
          f1 <- fibMemo (n - 1)
          f2 <- fibMemo (n - 2)
          let result = f1 + f2
          modify (M.insert n result)
          return result

computeFib :: Int -> Integer
computeFib n = evalState (fibMemo n) M.empty

--------------------------------------------------------------------------------
-- HC20T14: Monadic Filter-Map - Alternative
--------------------------------------------------------------------------------

filterMapM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
filterMapM _ [] = return []
filterMapM fn (x:xs) = do
  maybeResult <- fn x
  rest <- filterMapM fn xs
  return $ case maybeResult of
    Just y -> y : rest
    Nothing -> rest

--------------------------------------------------------------------------------
-- HC20T15: Tree with custom monad - Alternative
--------------------------------------------------------------------------------

data BinTree
  = Branch BinTree BinTree
  | Value Int
  deriving (Show, Eq)

newtype Accumulator a = Acc { extract :: (Int, a) }

instance Functor Accumulator where
  fmap fn (Acc (total, val)) = Acc (total, fn val)

instance Applicative Accumulator where
  pure val = Acc (0, val)
  Acc (s1, fn) <*> Acc (s2, val) = Acc (s1 + s2, fn val)

instance Monad Accumulator where
  Acc (s1, val) >>= fn =
    let Acc (s2, result) = fn val
    in Acc (s1 + s2, result)

recordSum :: Int -> Accumulator ()
recordSum n = Acc (n, ())

sumTree :: BinTree -> Accumulator Int
sumTree (Value n) = recordSum n >> return n
sumTree (Branch left right) = do
  leftSum <- sumTree left
  rightSum <- sumTree right
  return (leftSum + rightSum)

computeTreeSum :: BinTree -> (Int, Int)
computeTreeSum tree = extract (sumTree tree)

--------------------------------------------------------------------------------
-- HC20T16: Retry mechanism - Enhanced version
--------------------------------------------------------------------------------

attemptWithRetry :: forall a. Int -> IO a -> IO (Maybe a)
attemptWithRetry maxAttempts action
  | maxAttempts <= 0 = return Nothing
  | otherwise = do
      result <- (try action :: IO (Either SomeException a))
      case result of
        Right success -> return (Just success)
        Left _ -> attemptWithRetry (maxAttempts - 1) action

--------------------------------------------------------------------------------
-- HC20T17: Password validation - Enhanced
--------------------------------------------------------------------------------

checkPassword :: String -> Either [String] String
checkPassword password =
  let validations =
        [ (length password >= 8, "Minimum 8 characters required")
        , (any isDigit password, "Must include at least one number")
        , (any isUpper password, "Must include at least one uppercase letter")
        , (any isLower password, "Must include at least one lowercase letter")
        , (any (not . isAlpha) password, "Must include at least one special character")
        ]
      failures = [msg | (valid, msg) <- validations, not valid]
  in if null failures then Right password else Left failures

--------------------------------------------------------------------------------
-- HC20T18: MaybeT for user input - Alternative
--------------------------------------------------------------------------------

readNonEmpty :: String -> MaybeT IO String
readNonEmpty prompt = MaybeT $ do
  putStr prompt
  input <- getLine
  return $ if null (filter (not . isSpace) input)
           then Nothing
           else Just input

readInteger :: String -> MaybeT IO Int
readInteger prompt = MaybeT $ do
  putStr prompt
  input <- getLine
  return $ case reads input of
    [(num, "")] -> Just num
    _ -> Nothing

--------------------------------------------------------------------------------
-- HC20T19: Logging system with Writer - Enhanced
--------------------------------------------------------------------------------

logOperation1 :: Show a => String -> (a -> b) -> a -> Writer LogMsg b
logOperation1 name operation arg = do
  tell ["[" ++ name ++ "] input: " ++ show arg]
  let result = operation arg
  tell ["[" ++ name ++ "] completed"]
  return result

logOperation2 :: (Show a, Show b) => String -> (a -> b -> c) -> a -> b -> Writer LogMsg c
logOperation2 name operation arg1 arg2 = do
  tell ["[" ++ name ++ "] inputs: " ++ show arg1 ++ ", " ++ show arg2]
  let result = operation arg1 arg2
  tell ["[" ++ name ++ "] completed"]
  return result

--------------------------------------------------------------------------------
-- HC20T20: Batch processing - Alternative
--------------------------------------------------------------------------------

processBatch :: Monad m => (a -> m b) -> [a] -> m [b]
processBatch transform items =
  foldl' (\acc item -> do
            results <- acc
            result <- transform item
            return (results ++ [result]))
         (return [])
         items

-- Using mapM
processBatchSimple :: Monad m => (a -> m b) -> [a] -> m [b]
processBatchSimple = mapM

--------------------------------------------------------------------------------
-- Main demonstration
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== HC20T1: Safe Division ==="
  print (divSafe 10 (2 :: Double), divSafe 5 0)
  print (divChain 100 [2, 5, 2] :: Maybe Double)

  putStrLn "\n=== HC20T2: Sequence Maybe Values ==="
  print (allJust [Just 1, Just 2, Just 3] :: Maybe [Int])
  print (allJust [Just 1, Nothing, Just 3] :: Maybe [Int])
  print (allJustTraverse [Just 5, Just 10] :: Maybe [Int])

  putStrLn "\n=== HC20T3: Calculator with Logging ==="
  let (answer, logs) = runWriter (computeExpression 3 4 10 2)
  print answer
  mapM_ putStrLn logs

  putStrLn "\n=== HC20T4: Character Counting ==="
  print (countOccurrences 'a' "bananas")
  print (countMultiple ['a', 'n', 's'] "bananas")

  putStrLn "\n=== HC20T5: Reader Configuration ==="
  print (runReader (formatGreeting "Alice") standardConfig)
  print (runReader (formatGreeting "Bob") formalConfig)

  putStrLn "\n=== HC20T6: Combining Monads ==="
  print (pairMonads (Just 'X') [1..4 :: Int])
  print (pairMonads (Nothing :: Maybe Char) [1..4 :: Int])

  putStrLn "\n=== HC20T7: Find with Either ==="
  print (findElement even [1,3,4,6 :: Int])
  print (findElement (>10) [1,3,4,6 :: Int])

  putStrLn "\n=== HC20T8: Expression Parser ==="
  print (evaluateExpr "2 + 3 * 4")
  print (evaluateExpr "(2+3)*4")
  print (evaluateExpr " 10 + (6 * 5) + 1 ")

  putStrLn "\n=== HC20T9: Identity Monad ==="
  print (runIdentity (duplicateM 5 'a'))

  putStrLn "\n=== HC20T10: Stack with Transformers ==="
  print (executeStack (popMultiple 3) [10,20,30,40 :: Int])
  print (executeStack (popMultiple 5) [10,20,30,40 :: Int])

  putStrLn "\n=== HC20T11: Simulated Walk ==="
  print (take 10 (simulateWalk 15))

  putStrLn "\n=== HC20T12: File Reading (skipped) ==="
  putStrLn "Call displayFileContent \"path.txt\" to test"

  putStrLn "\n=== HC20T13: Fibonacci with Memoization ==="
  print (computeFib 20)

  putStrLn "\n=== HC20T14: Monadic Filter-Map ==="
  let transform n = return (if even n then Just (n*n) else Nothing) :: IO (Maybe Int)
  print =<< filterMapM transform [1..10 :: Int]

  putStrLn "\n=== HC20T15: Tree Sum with Custom Monad ==="
  let tree = Branch (Value 5) (Branch (Value 7) (Value 8))
  print (computeTreeSum tree)

  putStrLn "\n=== HC20T16: Retry with IO ==="
  outcome <- attemptWithRetry 3 (return (99 :: Int))
  print outcome

  putStrLn "\n=== HC20T17: Password Validation ==="
  print (checkPassword "Abcdefg1!")
  print (checkPassword "short")

  putStrLn "\n=== HC20T19: Writer Logging System ==="
  let (finalVal, logEntries) = runWriter $ do
        a <- logOperation1 "increment" succ (10 :: Int)
        logOperation2 "addition" (+) a 5
  print finalVal
  mapM_ putStrLn logEntries

  putStrLn "\n=== HC20T20: Batch Processing ==="
  results <- processBatch (\n -> putStrLn ("Processing " ++ show n) >> return (n*n)) [1..5 :: Int]
  print results
