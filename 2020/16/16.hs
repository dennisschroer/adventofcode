import           Data.List
import           Data.List.Split
import           Data.List.Utils
import qualified Data.Map        as Map

main = do
  test <- readFile "test"
  test2 <- readFile "test2"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ fields test
  print $ tickets test
  print $ sum $ concat $ map (findInvalidValues (fields test)) (tickets test)

  putStrLn "== Part 1 =="
  print $ sum $ concat $ map (findInvalidValues (fields input)) (tickets input)

  putStrLn "== Test Part 2 =="
  let ticketFields = fields test2
  let validTickets = filter (isValidTicket ticketFields) $ tickets test2
  print ticketFields
  print validTickets
  print $ map (possibleFields ticketFields) $ transpose validTickets
  print $ last $ take 5 $ iterate reducePossibilities $ map (possibleFields ticketFields) $ transpose validTickets

  putStrLn "== Part 2 =="
  let ticketFields = fields input
  let ticket = myTicket input
  let validTickets = filter (isValidTicket ticketFields) $ tickets input
  let fieldOrder = map head $ last $ take (length ticketFields) $ iterate reducePossibilities $ map (possibleFields ticketFields) $ transpose validTickets
  print ticket
  print fieldOrder
  print $ product $ map (\index -> ticket!!index) $ findIndices (isPrefixOf "departure") fieldOrder


fields :: String -> [(String, [Int])]
fields input = map parseField $ splitOn "\n" $ head $ splitOn "\n\n" input

myTicket :: String -> [Int]
myTicket input = map read $ splitOn "," $ last $ splitOn "\n" $ head $ drop 1 $ splitOn "\n\n" input

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

isValidTicket :: [(String, [Int])] -> [Int] -> Bool
isValidTicket fields ticket = null $ findInvalidValues fields ticket

possibleFields :: [(String, [Int])] -> [Int] -> [String]
possibleFields fields values = map fst $ filter (\f -> all (\v -> elem v (snd f)) values) fields

reducePossibilities :: [[String]] -> [[String]]
reducePossibilities possibilities =
  let
    singletonIndices = findIndices (\p -> 1==length p) possibilities
    singletons = foldl (\a i -> a ++ (possibilities!!i)) [] singletonIndices
    in map (\fields -> if (all (\f -> elem f singletons) fields) then fields else (fields \\ singletons)) possibilities
