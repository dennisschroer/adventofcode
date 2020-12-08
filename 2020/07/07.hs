import Data.Char
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Maybe

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ canHoldBag "shiny gold" $ parse test
  print $ length $ canHoldBag "shiny gold" $ parse test

  putStrLn "== Part 1 =="
  print $ canHoldBag "shiny gold" $ parse input
  print $ length $ canHoldBag "shiny gold" $ parse input

  putStrLn "== Test Part 2 =="
  print $ bagContents "shiny gold" $ parse test
  print $ countContents $ bagContents "shiny gold" $ parse test

  putStrLn "== Part 2 =="
  print $ bagContents "shiny gold" $ parse input
  print $ countContents $ bagContents "shiny gold" $ parse input

parse :: String -> [(String, [(String, Int)])]
parse input = map parseLine $ map (replace "." "" . replace " bag" "" . replace " bags" "") $ lines input

parseLine :: String -> (String, [(String, Int)])
parseLine line = let l = splitOn " contain " line in (head l, parseBags $ last l)

parseBags :: String -> [(String, Int)]
parseBags "no other" = []
parseBags bagsLine = (\bags -> [(dropWhile (\c -> not $ isLetter c) b, (read::String->Int) $ takeWhile isDigit b) | b <- bags ] ) $ splitOn ", " bagsLine

canHoldBagDirectly :: String -> [(String, [(String, Int)])] -> [String]
canHoldBagDirectly bag rules = map fst $ filter (\(b, contents) -> maybe False (>0) $ lookup bag contents) rules

canHoldBag :: String -> [(String, [(String, Int)])] -> [String]
canHoldBag bag rules =
  let directContainers = canHoldBagDirectly bag rules
  in uniq $ (concat $ map (\b -> canHoldBag b rules) directContainers) ++ directContainers

bagContents :: String -> [(String, [(String, Int)])] -> [(String, Int)]
bagContents bag rules =
  let directContents = fromJust $ lookup bag rules
  in (concat $ map (\(b, c) -> map (\(b2, c2) -> (b2, c*c2)) $ bagContents b rules) directContents) ++ directContents

countContents :: [(String, Int)] -> Int
countContents contents = sum $ map snd contents
