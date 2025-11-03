-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)


-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)


-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty = Empty
addN n (Has x) = Has (x + n)


-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty = def
extract _ (Has x) = x


-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a, radius :: Float }
              | Rectangle { color :: a, width :: Float, height :: Float }
              deriving (Show)


-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet {
    content :: String,
    likes :: Int,
    comments :: [Tweet]
} deriving (Show)


-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ likes comments) =
    likes + sum (map engagement comments)


-- HC9T8: Recursive Sequence Data Type
data Sequence a = Nil | Node a (Sequence a)
    deriving (Show)


-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ Nil = False
elemSeq x (Node y rest)
    | x == y    = True
    | otherwise = elemSeq x rest


-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree
           | NodeTree a (BST a) (BST a)
           deriving (Show)


-- Example Usage (for testing in your Haskell editor)
main :: IO ()
main = do
    -- HC9T3: Add values in Box
    print (addN 5 (Has 10))     -- Has 15
    print (addN 3 Empty)        -- Empty

    -- HC9T4: Extract values
    print (extract 0 (Has 7))   -- 7
    print (extract 0 Empty)     -- 0

    -- HC9T5: Shapes
    let c = Circle "Red" 5.0
    let r = Rectangle "Blue" 4.0 6.0
    print c
    print r

    -- HC9T6–7: Tweets and Engagement
    let t1 = Tweet "Hello world" 10 []
    let t2 = Tweet "Reply" 5 [t1]
    print (engagement t2)       -- 15

    -- HC9T8–9: Sequence
    let seq1 = Node 1 (Node 2 (Node 3 Nil))
    print (elemSeq 2 seq1)      -- True
    print (elemSeq 5 seq1)      -- False

    -- HC9T10: BST example
    let tree = NodeTree 10 (NodeTree 5 EmptyTree EmptyTree) (NodeTree 15 EmptyTree EmptyTree)
    print tree
