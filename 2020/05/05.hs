import Data.List

main = do
  input <- readFile "input"

  -- Test input
  putStrLn "== Test =="
  print $ codeToRow "BFFFBBFRRR"
  print $ codeToSeat "BFFFBBFRRR"
  print $ seatId "BFFFBBFRRR"
  print $ codeToRow "FFFBBBFRRR"
  print $ codeToSeat "FFFBBBFRRR"
  print $ seatId "FFFBBBFRRR"
  print $ codeToRow "BBFFBBFRLL"
  print $ codeToSeat "BBFFBBFRLL"
  print $ seatId "BBFFBBFRLL"

  putStrLn "== Part 1 =="
  print $ maximum $ map seatId $ lines input

  putStrLn "== Part 2 =="
  print $ [54..930] \\ (map seatId $ lines input)

codeToRow :: String -> Int
codeToRow code = sum $ map (2^) $ findIndices (=='B') $ reverse $ take 7 code

codeToSeat :: String -> Int
codeToSeat code = sum $ map (2^) $ findIndices (=='R') $ reverse $ drop 7 code

seatId :: String -> Int
seatId code = (codeToRow code) * 8 + (codeToSeat code)
