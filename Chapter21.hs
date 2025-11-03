{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char (toUpper)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import Data.Semigroup ((<>))
import Data.Monoid (Sum(..))

--------------------------------------------------------------------------------
-- Writer Monad
--------------------------------------------------------------------------------
newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  Writer (f, w1) <*> Writer (a, w2) = Writer (f a, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  Writer (a, w1) >>= k =
    let Writer (b, w2) = k a
    in Writer (b, w1 <> w2)

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen (Writer (a, w)) = Writer ((a, w), w)

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)

--------------------------------------------------------------------------------
-- Reader Monad
--------------------------------------------------------------------------------
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  Reader rf <*> Reader ra = Reader (\r -> rf r (ra r))

instance Monad (Reader r) where
  Reader ra >>= k = Reader $ \r -> runReader (k (ra r)) r

ask :: Reader r r
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader (ra . f)

--------------------------------------------------------------------------------
-- State Monad
--------------------------------------------------------------------------------
newtype State s a = MkState { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (MkState run) = MkState $ \s -> let (a, s') = run s in (f a, s')

instance Applicative (State s) where
  pure a = MkState (\s -> (a, s))
  MkState rf <*> MkState ra = MkState $ \s ->
    let (f, s1) = rf s
        (a, s2) = ra s1
    in (f a, s2)

instance Monad (State s) where
  MkState ra >>= k = MkState $ \s ->
    let (a, s1) = ra s
    in runState (k a) s1

get :: State s s
get = MkState (\s -> (s, s))

put :: s -> State s ()
put s = MkState (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = MkState (\s -> ((), f s))

evalState :: State s a -> s -> a
evalState st s0 = fst (runState st s0)

execState :: State s a -> s -> s
execState st s0 = snd (runState st s0)

--------------------------------------------------------------------------------
-- Writer Logging Calculator
--------------------------------------------------------------------------------
addW, subW, mulW :: Int -> Int -> Writer [String] Int
addW x y = let r = x + y in tell ["add " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r
subW x y = let r = x - y in tell ["sub " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r
mulW x y = let r = x * y in tell ["mul " ++ show x ++ " " ++ show y ++ " = " ++ show r] >> pure r

calcDemo :: Writer [String] Int
calcDemo = do
  a <- addW 3 4
  b <- mulW a 10
  subW b 5

--------------------------------------------------------------------------------
-- Redacting Secrets
--------------------------------------------------------------------------------
redactSecrets :: Writer [String] a -> Writer [String] a
redactSecrets w =
  pass $ do
    (a, logs) <- listen w
    let f = map (\ln -> if "SECRET" `isInfixOf` map toUpper ln then "[REDACTED]" else ln)
    pure (a, f)

--------------------------------------------------------------------------------
-- Reader Configurable Greeting
--------------------------------------------------------------------------------
data Config = Config { greetPrefix :: String, shout :: Bool } deriving (Show)

greet :: String -> Reader Config String
greet name = do
  cfg <- ask
  let base = greetPrefix cfg ++ " " ++ name
  pure $ if shout cfg then map toUpper base ++ "!" else base

greetWithFlip :: String -> Reader Config (String, String)
greetWithFlip name = do
  normal <- greet name
  flipped <- local (\c -> c { shout = not (shout c) }) (greet name)
  pure (normal, flipped)

--------------------------------------------------------------------------------
-- State Vending Machine
--------------------------------------------------------------------------------
data VendingState = MkVendingState { items :: Int, credit :: Int } deriving (Show)

price :: Int
price = 7

insertCoin :: Int -> State VendingState ()
insertCoin c = do
  s <- get
  put s { credit = credit s + c }

vend :: State VendingState String
vend = do
  s <- get
  if items s <= 0
     then pure "Sold out"
     else if credit s < price
            then pure "Insufficient credit"
            else do
              put s { items = items s - 1, credit = credit s - price }
              pure "Vended"

getChange :: State VendingState Int
getChange = do
  s <- get
  put s { credit = 0 }
  pure (credit s)

vendingSequence :: State VendingState (String, Int)
vendingSequence = do
  insertCoin 5
  insertCoin 5
  msg <- vend
  chg <- getChange
  pure (msg, chg)

--------------------------------------------------------------------------------
-- State Undo Stack
--------------------------------------------------------------------------------
setValue :: Int -> State (Int, [Int]) ()
setValue new = do
  (cur, hist) <- get
  put (new, cur:hist)

undo :: State (Int, [Int]) ()
undo = do
  (cur, hist) <- get
  case hist of
    (h:hs) -> put (h, hs)
    []     -> pure ()

undoScript :: State (Int,[Int]) (Int, [Int])
undoScript = do
  setValue 10
  setValue 20
  setValue 30
  undo
  undo
  get

--------------------------------------------------------------------------------
-- State Random Walk
--------------------------------------------------------------------------------
randomStep :: State (Int, (Int, Int)) ()
randomStep = MkState $ \(seed, (x,y)) ->
  let seed' = (1103515245 * seed + 12345) `mod` 2147483648
      dir   = seed' `mod` 4
      (dx,dy) = case dir of
        0 -> (0, 1)
        1 -> (0,-1)
        2 -> (-1,0)
        _ -> (1, 0)
      pos' = (x + dx, y + dy)
  in ((), (seed', pos'))

randomWalk :: Int -> State (Int, (Int, Int)) [(Int, Int)]
randomWalk n = do
  (_, start) <- get
  go n start [start]
  where
    go 0 _ path = pure (reverse path)
    go k _ path = do
      randomStep
      (_, p') <- get
      go (k-1) p' (p':path)

--------------------------------------------------------------------------------
-- Reader + Writer Configurable Logging
--------------------------------------------------------------------------------
step :: String -> Reader Config (Writer [String] ())
step msg = do
  cfg <- ask
  let prefix = if shout cfg then map toUpper (greetPrefix cfg) else greetPrefix cfg
  pure $ tell [prefix ++ ": " ++ msg]

stepsDemo :: Reader Config (Writer [String] ())
stepsDemo = do
  a <- step "connect"
  b <- step "run query"
  c <- local (\c -> c { shout = True, greetPrefix = "dbg" }) (step "debug info")
  pure (a >> b >> c)

--------------------------------------------------------------------------------
-- State + Writer Instrumented State
--------------------------------------------------------------------------------
inc :: Int -> State Int (Writer [String] Int)
inc n = do
  modify (+ n)
  s <- get
  pure (tell ["inc " ++ show n ++ " -> " ++ show s] >> pure s)

dec :: Int -> State Int (Writer [String] Int)
dec n = do
  modify (subtract n)
  s <- get
  pure (tell ["dec " ++ show n ++ " -> " ++ show s] >> pure s)

instrumentedDemo :: State Int (Writer [String] Int)
instrumentedDemo = do
  _ <- inc 5
  _ <- dec 2
  inc 10

--------------------------------------------------------------------------------
-- Reader + State Ticks
--------------------------------------------------------------------------------
tick :: Reader Config (State Int Bool)
tick = do
  cfg <- ask
  pure $ do
    modify (+1)
    s <- get
    pure (s >= length (greetPrefix cfg) + (if shout cfg then 1 else 0))

runTicks :: Int -> Reader Config [Bool]
runTicks n = do
  t <- tick
  pure $ evalState (sequence [ t | _ <- [1..n] ]) 0

--------------------------------------------------------------------------------
-- Legacy Refactor
--------------------------------------------------------------------------------
type Env = M.Map String String

legacyRefactored :: Reader Env (State Int (Writer [String] Int))
legacyRefactored = do
  env <- ask
  pure $ do
    s0 <- get
    let startMsg = "starting at " ++ show s0
        portMsg  = "using port " ++ M.findWithDefault "5432" "DB_PORT" env
    modify (+1)
    s1 <- get
    pure $ do
      tell [startMsg, portMsg, "counter = " ++ show s1]
      pure s1

--------------------------------------------------------------------------------
-- Main Demo
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "=== Writer Logging Calculator ==="
  print $ runWriter calcDemo

  putStrLn "\n=== Redacting Secrets ==="
  print $ runWriter (redactSecrets calcDemo)

  putStrLn "\n=== Reader Configurable Greeting ==="
  print $ runReader (greetWithFlip "Alice") (Config "Hello" False)

  putStrLn "\n=== State Vending Machine ==="
  print $ runState vendingSequence (MkVendingState 3 0)

  putStrLn "\n=== State Undo Stack ==="
  print $ runState undoScript (0,[])

  putStrLn "\n=== State Random Walk ==="
  print $ evalState (randomWalk 5) (42, (0,0))

  putStrLn "\n=== Reader + Writer Configurable Logging ==="
  let ((), logsRW) = runWriter (runReader stepsDemo (Config "app" False))
  mapM_ print logsRW

  putStrLn "\n=== State + Writer Instrumented State ==="
  let (w, finalS) = runState instrumentedDemo 0
      (lastVal, logs2) = runWriter w
  print finalS
  print lastVal
  mapM_ putStrLn logs2

  putStrLn "\n=== Reader + State Ticks ==="
  let cfgA = Config "abc" False
      cfgB = Config "abc" True
  print (runReader (runTicks 6) cfgA)
  print (runReader (runTicks 6) cfgB)

  putStrLn "\n=== Legacy Refactor Demo ==="
  let (w3, st1) = runState (runReader legacyRefactored (M.fromList [("DB_PORT","15432")])) 0
      (resultCounter, logs3) = runWriter w3
  print st1
  print resultCounter
  mapM_ putStrLn logs3
