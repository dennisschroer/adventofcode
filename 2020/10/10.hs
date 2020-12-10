import           Data.List
import           Data.List.Utils

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ product $ map length $ group $ sort $ (\l -> 3 : zipWith (-) (drop 1 l) l) $ sort $ 0 : parse test

  putStrLn "== Part 1 =="
  print $ product $ map length $ group $ sort $ (\l -> 3 : zipWith (-) (drop 1 l) l) $ sort $ 0 : parse input

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

parse :: String -> [Int]
parse input =  map read $ lines input
