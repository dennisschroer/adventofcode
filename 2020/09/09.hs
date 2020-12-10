import           Data.List
import           Data.List.Utils

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ head $ findInvalidNumbers (parse test) 5

  putStrLn "== Part 1 =="
  print $ head $ findInvalidNumbers (parse input) 25

  putStrLn "== Test Part 2 =="
  print $ head $ findWeakness (parse test) 5

  putStrLn "== Part 2 =="
  print $ head $ findWeakness (parse input) 25

parse :: String -> [Int]
parse input =  map read $ lines input

isSumOfPreviousPairs :: [Int] -> Int -> Int -> Bool
isSumOfPreviousPairs numbers size index =
  let
    number = numbers !! index
    previousNumbers = drop (index-size) $ take index numbers
    pairs = [(x,y) | (x:ys) <- tails previousNumbers, y <- ys ]
    sums = map (\p -> fst p + snd p) pairs
  in elem number sums

findInvalidIndices :: [Int] -> Int -> [Int]
findInvalidIndices numbers size = filter (not . isSumOfPreviousPairs numbers size) [size..(length numbers)-1]

findInvalidNumbers :: [Int] -> Int -> [Int]
findInvalidNumbers numbers size = map (numbers!!) $ findInvalidIndices numbers size

findWeakness numbers size =
  let
    index = head $ findInvalidIndices numbers size
    number = numbers !! index
    ranges = filter (\s -> number==sum s) $ (concat . map inits . tails) $ take index numbers
  in map (\s -> minimum s + maximum s) ranges
