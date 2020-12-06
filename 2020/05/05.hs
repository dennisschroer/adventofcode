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
  print $ maximum $ map seatId2 $ lines input

  -- Or, in one line:
  print $ maximum $ map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines input

  putStrLn "== Part 2 =="
  print $ [54..930] \\ (map seatId $ lines input)
  print $ [54..930] \\ (map seatId2 $ lines input)

  -- Or, in one line:
  print $ [54..930] \\ (map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines input)

codeToRow :: String -> Int
codeToRow code = sum $ map (2^) $ elemIndices 'B' $ reverse $ take 7 code

codeToSeat :: String -> Int
codeToSeat code = sum $ map (2^) $ elemIndices 'R' $ reverse $ drop 7 code

seatId :: String -> Int
seatId code = (codeToRow code) * 8 + (codeToSeat code)

-- No need to split the code first: the entire code is one binary number, with B and R as 1:
seatId2 :: String -> Int
seatId2 code = sum $ map (2^) $ findIndices (\c -> elem c "BR") $ reverse code
