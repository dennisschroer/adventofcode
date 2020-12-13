import           Data.List
import           Data.List.Split
import           Data.List.Utils

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ part1 test

  putStrLn "== Part 1 =="
  print $ part1 input

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

part1 :: String -> Int
part1 input =
  let timestamp         = read $ head $ lines input
      busses            = map read $ filter (/="x") $ splitOn "," $ last $ lines input
      first             = firstBus busses timestamp
      waitTimeForFirst  = waitTime first timestamp
  in first * waitTimeForFirst

waitTime :: Int -> Int -> Int
waitTime busId timestamp = busId - (timestamp `mod` busId)

firstBus:: [Int] -> Int -> Int
firstBus busses timestamp = minimumBy (\a b -> compare (waitTime a timestamp) (waitTime b timestamp)) busses
