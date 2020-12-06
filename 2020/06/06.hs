import Data.List
import Data.List.Split

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test for part 1 =="
  print $ map (combinedAnswersUnion . lines) $ splitOn "\n\n" test
  print $ sum $ map (length . combinedAnswersUnion . lines) $ splitOn "\n\n" test

  putStrLn "== Test for part 2 =="
  print $ map (combinedAnswersIntersection . lines) $ splitOn "\n\n" test
  print $ sum $ map (length . combinedAnswersIntersection . lines) $ splitOn "\n\n" test

  putStrLn "== Part 1 =="
  print $ sum $ map (length . combinedAnswersUnion . lines) $ splitOn "\n\n" input

  -- Oneliner
  print $ sum $ map (length . (foldl union "") . lines) $ splitOn "\n\n" input

  putStrLn "== Part 2 =="
  print $ sum $ map (length . combinedAnswersIntersection . lines) $ splitOn "\n\n" input

  -- Oneliner
  print $ sum $ map (length . (foldl intersect ['a'..'z']) . lines) $ splitOn "\n\n" input

combinedAnswersUnion :: [String] -> String
combinedAnswersUnion answers = foldl union "" answers

combinedAnswersIntersection :: [String] -> String
combinedAnswersIntersection answers = foldl intersect ['a'..'z'] answers
