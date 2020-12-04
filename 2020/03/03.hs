getCharAt :: [[Char]] -> (Int, Int) -> Char
getCharAt field (x,y) =
  let width = length $ field !! 0
  in field !! y !! (x `mod` width)

calculateAmountOfTrees :: String -> Int -> Int -> Int
calculateAmountOfTrees input right down =
  let
    height = length $ lines input
    width = length $ head $ lines input
    steps = [0..((height `div` down) - 1)]
  in length $ filter ('#'==) $ [ lines input !! (i*down) !! (mod (i*right) width) | i <- steps]

main = do
  input <- readFile "input"

  -- Part 1, long version
  let field = lines input

  let step_down = 1
  let step_right = 3
  let height = length $ lines input
  let width = length $ lines input !! 0

  let steps = [0..((height `div` step_down) - 1)]
  let coordinates = [(i*step_right, i*step_down) | i <- steps]

  let chars = map (getCharAt field) coordinates

  print $ length $ filter ('#'==) chars

  let height = length field
  let width = length $ field !! 0

  -- Part 1, short version
  print $ length $ filter ('#'==) $ [ lines input !! i !! (mod (i*3) $ length $ head $ lines input) | i <- [0..((length $ lines input) - 1)]]

  -- Part 1, with function
  print $ calculateAmountOfTrees input 3 1

  -- Part 2
  let slope_1_1 = calculateAmountOfTrees input 1 1
  let slope_3_1 = calculateAmountOfTrees input 3 1
  let slope_5_1 = calculateAmountOfTrees input 5 1
  let slope_7_1 = calculateAmountOfTrees input 7 1
  let slope_1_2 = calculateAmountOfTrees input 1 2
  print slope_1_1
  print slope_3_1
  print slope_5_1
  print slope_7_1
  print slope_1_2
  print $ slope_1_1 * slope_3_1 * slope_5_1 * slope_7_1 * slope_1_2

  -- Or, in one line
  print $ foldl (*) 1 $ map (\(x,y) -> calculateAmountOfTrees input x y) [(1,1), (3,1), (5,1), (7,1), (1,2)]
