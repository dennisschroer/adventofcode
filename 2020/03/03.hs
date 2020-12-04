

getCharAt :: [[Char]] -> (Int, Int) -> Char
getCharAt field (x,y) =
  let width = length $ field !! 0
  in field !! y !! (x `mod` width)

calculateAmountOfTrees :: String -> Int -> Int -> Int
calculateAmountOfTrees contents right down =
  let
    height = length $ lines contents
    width = length $ head $ lines contents
    steps = [0..((height `div` down) - 1)]
  in length $ filter ('#'==) $ [ lines contents !! (i*down) !! (mod (i*right) width) | i <- steps]

main = do
  -- Part 1, long version
  contents <- readFile "input"
  let field = lines contents

  let step_down = 1
  let step_right = 3
  let height = length $ lines contents
  let width = length $ lines contents !! 0

  let steps = [0..((height `div` step_down) - 1)]
  let coordinates = [(i*step_right, i*step_down) | i <- steps]

  let chars = map (getCharAt field) coordinates

  print $ length $ filter ('#'==) chars

  let height = length field
  let width = length $ field !! 0

  -- Part 1, short version
  contents <- readFile "input"
  print $ length $ filter ('#'==) $ [ lines contents !! i !! (mod (i*3) $ length $ head $ lines contents) | i <- [0..((length $ lines contents) - 1)]]

  -- Part 1, with function
  contents <- readFile "input"
  print $ calculateAmountOfTrees contents 3 1

  -- Part 2
  let slope_1_1 = calculateAmountOfTrees contents 1 1
  let slope_3_1 = calculateAmountOfTrees contents 3 1
  let slope_5_1 = calculateAmountOfTrees contents 5 1
  let slope_7_1 = calculateAmountOfTrees contents 7 1
  let slope_1_2 = calculateAmountOfTrees contents 1 2
  print slope_1_1
  print slope_3_1
  print slope_5_1
  print slope_7_1
  print slope_1_2
  print $ slope_1_1 * slope_3_1 * slope_5_1 * slope_7_1 * slope_1_2
