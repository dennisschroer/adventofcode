import           Data.List
import           Data.List.Split
import           Data.List.Utils
import qualified Data.Map        as Map

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ fields test
  print $ tickets test
  print $ sum $ concat $ map (findInvalidValues (fields test)) (tickets test)

  putStrLn "== Part 1 =="
  print $ sum $ concat $ map (findInvalidValues (fields input)) (tickets input)

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

fields :: String -> [(String, [Int])]
fields input = map parseField $ splitOn "\n" $ head $ splitOn "\n\n" input

tickets :: String -> [[Int]]
tickets input = map (map read . splitOn ",") $ tail $ init $ splitOn "\n" $ last $ splitOn "\n\n" input

parseField :: String -> (String, [Int])
parseField field =
  let
    name = head $ splitOn ": " field
    ranges = splitOn " or " $ last $ splitOn ": " field
    numbers = foldl (\result range -> result ++ (parseRange range)) [] ranges
    in (name, numbers)

parseRange :: String -> [Int]
parseRange range =
  let
    parts = splitOn "-" range
    in [(read $ head parts)..(read $ last parts)]

findInvalidValues :: [(String, [Int])] -> [Int] -> [Int]
findInvalidValues fields ticket = filter (\value -> all (notElem value) (map snd fields)) ticket
