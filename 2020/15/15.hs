import           Data.List
import           Data.List.Split
import           Data.List.Utils
import qualified Data.Map        as Map

main = do
  let test = "0,3,6"
  let input = "1,2,16,19,18,0"

  putStrLn "== Test Part 1 =="
  print $ convert $ map read $ splitOn "," "0,3,6"
  print (4, 0, doNTurns 4 $ convert $ map read $ splitOn "," "0,3,6")
  print (5, 3, doNTurns 5 $ convert $ map read $ splitOn "," "0,3,6")
  print (6, 3, doNTurns 6 $ convert $ map read $ splitOn "," "0,3,6")
  print (7, 1, doNTurns 7 $ convert $ map read $ splitOn "," "0,3,6")
  print (8, 0, doNTurns 8 $ convert $ map read $ splitOn "," "0,3,6")
  print (9, 4, doNTurns 9 $ convert $ map read $ splitOn "," "0,3,6")
  print (10, 0, doNTurns 10 $ convert $ map read $ splitOn "," "0,3,6")

  print (436, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "0,3,6")
  print (1, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "1,3,2")
  print (10, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "2,1,3")
  print (27, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "1,2,3")
  print (78, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "2,3,1")
  print (438, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "3,2,1")
  print (1836, lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," "3,1,2")

  putStrLn "== Part 1 =="
  print $ lastTurn $ doNTurns 2020 $ convert $ map read $ splitOn "," input

  putStrLn "== Test Part 2 =="
  print (175594, lastTurn $! doNTurns 30000000 $! convert $ map read $ splitOn "," "0,3,6")
  --print (2578, fst $ doNTurns 30000000 $ convert $ map read $ splitOn "," "1,3,2")
  -- print (3544142, fst $  doNTurns 30000000 $ convert $ map read $ splitOn "," "2,1,3")
  -- print (261214, fst $ doNTurns 30000000 $ convert $ map read $ splitOn "," "1,2,3")
  -- print (6895259, fst $ doNTurns 30000000 $ convert $ map read $ splitOn "," "2,3,1")
  -- print (18, fst $ doNTurns 30000000 $ convert $ map read $ splitOn "," "3,2,1")
  -- print (362,fst $  doNTurns 30000000 $ convert $ map read $ splitOn "," "3,1,2")

  putStrLn "== Part 2 =="
  -- print $ fst $ doNTurns 30000000 $ convert $ map read $ splitOn "," input

-- Tuple containing last turn, turns taken, and postition of previous turns
convert :: [Int] -> (Int, Int, Map.Map Int Int)
convert numbers = (last numbers, length numbers, foldl (\m turn -> Map.insert (numbers!!(turn-1)) turn m) Map.empty [1..length numbers])

doTurn :: Int -> (Int, Int, Map.Map Int Int) -> (Int, Int, Map.Map Int Int)
doTurn turn (lastTurn, turnCount, indices) = (maybe 0 (\pos -> turn - pos - 1) (Map.lookup lastTurn indices), turnCount + 1, Map.insert lastTurn (turn-1) indices)

doNTurns :: Int -> (Int, Int, Map.Map Int Int) -> (Int, Int, Map.Map Int Int)
doNTurns turns (lastTurn, turnsTaken, start) = foldl' (\state turn -> doTurn turn state) (lastTurn, turnsTaken, start) [turnsTaken + 1..turns]

lastTurn :: (Int, Int, Map.Map Int Int) -> Int
lastTurn (l,_,_) = l



nextNumber :: [Int] -> Int
nextNumber numbers = maybe 0 (\pos -> (length numbers) - pos - 1) $ elemRIndex (last numbers) (init numbers)

addNextNumber :: [Int] -> [Int]
addNextNumber numbers = (numbers ++ [nextNumber numbers])

numberAtTurn :: Int -> [Int] -> Int
numberAtTurn turn numbers = last $ foldl (\n _ -> addNextNumber n) numbers [length numbers..turn-1]
