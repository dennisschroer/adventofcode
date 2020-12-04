

getCharAt :: [[Char]] -> (Int, Int) -> Char
getCharAt field (x,y) =
  let width = length $ field !! 0
  in field !! y !! (x `mod` width)

main = do
  contents <- readFile "input"
  let field = lines contents

  let step_down = 1
  let step_right = 3

  let height = length field
  let width = length $ field !! 0

  let steps = [0..((height `div` step_down) - 1)]
  let coordinates = [(i*step_right, i*step_down) | i <- steps]

  let chars = map (getCharAt field) coordinates

  print $ length $ filter ('#'==) chars
