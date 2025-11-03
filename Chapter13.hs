import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Char (toLower)
import Data.Maybe (mapMaybe)

-- Task 4 & 5: Safe sum function with error handling
safeSumList :: [Int] -> Int
safeSumList [] = error "Empty list provided - cannot compute sum!"
safeSumList xs = foldl (+) 0 xs

-- Helper function for case-insensitive search
containsIgnoreCase :: String -> String -> Bool
containsIgnoreCase needle haystack = 
    map toLower needle `L.isInfixOf` map toLower haystack

-- Filter and sort files by search term
findAndSortFiles :: String -> [String] -> [String]
findAndSortFiles term files = 
    L.sort $ filter (containsIgnoreCase term) files

main :: IO ()
main = do
    putStrLn "=== Task 1: Number Sorting ==="
    let numberList = [5, 1, 9, 3, 7]
    putStrLn $ "Original: " ++ show numberList
    putStrLn $ "Sorted: " ++ show (L.sort numberList)
    
    putStrLn "\n=== Task 2: Map Creation ==="
    let fruitInventory = M.fromList [("apple", 3), ("banana", 5), ("cherry", 2)]
    putStrLn $ "Inventory: " ++ show fruitInventory
    
    putStrLn "\n=== Task 3: Sorted Map Keys ==="
    let sortedKeys = L.sort $ M.keys fruitInventory
    putStrLn $ "Keys in order: " ++ show sortedKeys
    
    putStrLn "\n=== Task 4 & 5: Safe Sum Function ==="
    let values1 = [5, 15, 25]
    putStrLn $ "Summing " ++ show values1 ++ " = " ++ show (safeSumList values1)
    
    let values2 = [0, 0, 0]
    putStrLn $ "Summing " ++ show values2 ++ " = " ++ show (safeSumList values2)
    
    -- To test error: uncomment below
    -- putStrLn $ "Summing [] = " ++ show (safeSumList [])
    
    putStrLn "\n=== Task 6: Simulated Directory ==="
    let fileList = ["notes.txt", "report.txt", "image.png", "todo.txt", "data.csv"]
    putStrLn $ "Files available: " ++ show fileList
    
    putStrLn "\n=== Task 7 & 8: Search and Sort Files ==="
    let query = "txt"
    let results = findAndSortFiles query fileList
    putStrLn $ "Files matching '" ++ query ++ "': " ++ show results
    
    putStrLn "\n=== Task 9: Empty Search Results ==="
    let emptyQuery = "pdf"
    let emptyResults = findAndSortFiles emptyQuery fileList
    case emptyResults of
        [] -> putStrLn $ "No files found matching '" ++ emptyQuery ++ "'"
        xs -> putStrLn $ "Found: " ++ show xs
    
    putStrLn "\n=== Task 10: Complete Demonstration ==="
    putStrLn "Combining all operations:"
    putStrLn $ "  - Map keys: " ++ show sortedKeys
    putStrLn $ "  - Sum example: " ++ show (safeSumList [10, 20, 30])
    putStrLn $ "  - Filtered files: " ++ show results
