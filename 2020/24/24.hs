import           Data.List
import           Data.List.Utils

data Direction = East | SouthEast | SouthWest | West | NorthWest | NorthEast deriving (Eq, Ord, Show)

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ normalize $ splitDirections $ "esew"
  print $ normalize $ splitDirections $ "nwwswee"

  print $ map splitDirections $ lines test
  print $ length $ removeDuplicates $ map (normalize . splitDirections) $ lines test

  putStrLn "== Part 1 =="
  print $ length $ removeDuplicates $ map (normalize . splitDirections) $ lines input

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

splitDirections :: String -> [Direction]
splitDirections []             =[]
splitDirections ('e':rest)     = East:(splitDirections rest)
splitDirections ('s':'e':rest) = SouthEast:(splitDirections rest)
splitDirections ('s':'w':rest) = SouthWest:(splitDirections rest)
splitDirections ('w':rest)     = West:(splitDirections rest)
splitDirections ('n':'w':rest) = NorthWest:(splitDirections rest)
splitDirections ('n':'e':rest) = NorthEast:(splitDirections rest)

loops = [
    [East,West],
    [NorthWest,SouthEast],
    [NorthEast,SouthWest],
    [NorthWest,SouthWest,East],
    [NorthEast,SouthEast,West]
  ]

shortenings = [
    ([NorthEast,SouthEast], East),
    ([East,SouthWest], SouthEast),
    ([West,SouthEast], SouthWest),
    ([NorthWest,SouthWest], West),
    ([West,NorthEast], NorthWest),
    ([East,NorthWest], NorthEast)
  ]

-- Iteratively remove loops and shorten paths. 20 iterations should be enough. Sort the result
normalize :: [Direction] -> [Direction]
normalize directions = sort $ last $ take 20 $ iterate (shorten . removeLoops) directions

-- Try to remove one loop of each type
removeLoops :: [Direction] -> [Direction]
removeLoops directions = foldl
  (\result loop ->
    if all (\d -> elem d result) loop
    then foldl (\r d -> delete d r) result loop
    else result
  ) directions loops

-- Try to shorten one path of eacth type
shorten :: [Direction] -> [Direction]
shorten directions = foldl
  (\result (path, replacement) ->
    if all (\d -> elem d result) path
    then foldl (\r d -> delete d r) (replacement:result) path
    else result
  ) directions shortenings

removeDuplicates :: [[Direction]] -> [[Direction]]
removeDuplicates paths = map head $ filter (\g -> odd $ length g) $ group $ sort paths
