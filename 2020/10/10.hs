import           Data.List
import           Data.List.Utils

main = do
  test_small <- readFile "test_small"
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ product $ map length $ group $ sort $ joltDifferences $ parse test

  putStrLn "== Part 1 =="
  print $ product $ map length $ group $ sort $ joltDifferences $ parse input

  putStrLn "== Test Part 2 =="
  print $ arrangementCount $ joltDifferences $ parse test_small
  print $ arrangementCount $ joltDifferences $ parse test
  print $ arrangementCount2 $ parse test_small
  print $ arrangementCount2 $ parse test

  putStrLn "== Part 2 =="
  print $ arrangementCount2 $ parse input

parse :: String -> [Int]
parse input =  map read $ lines input

joltDifferences :: [Int] -> [Int]
joltDifferences adapters = (\l -> (zipWith (-) (drop 1 l) l) ++ [3]) $ sort $ 0 : adapters

-- First approach using recursion. Takes way too long
-- This counts the number of arrangement based on the differences of jolts between adapters
arrangementCount :: [Int] -> Int
-- Base case: there is only 1 adapter, so only one arrangment
arrangementCount (x:[]) = 1
arrangementCount (x:y:zs)
  -- First adapter can be removed, or not. Arrangementcount is count of tail of both cases
  | (x+y) < 4 = (arrangementCount (y:zs)) + (arrangementCount ((x+y):zs))
  -- First adapter can not be removed
  | otherwise = (arrangementCount (y:zs))

-- Second approach (example is for first test case)
-- Step 1: calculate differences: [1,3,1,1,1,3,1,1,3,1,3,3]
-- Step 2: Count the number of consecutive 1s: [1,3,2,1]
-- Step 3: For each 2, there are 2 options. For each 3, there are 4 options. For each 4, there are 7 options: [1,4,2,1]
-- Step 4: Multiply all options: 8
arrangementCount2 :: [Int] -> Int
arrangementCount2 adapters = product $ replace [3] [4] $ replace [4] [7] $ map length $ filter (elem 1) $ group $ joltDifferences adapters
