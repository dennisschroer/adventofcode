import           Control.Parallel (par, pseq)
import           Data.List
import           Data.List.Utils

-- To speed up the calculation, compile with threaded execution:
-- Compile with: ghc -threaded -O2 24.hs
-- Run with: ./24 +RTS -N8


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
  print $ length $ removeDuplicates $ map (normalize . splitDirections) $ lines test
  print $ length $ flipTiles $ removeDuplicates $ map (normalize . splitDirections) $ lines test

  print $ map length $ take 101 $ iterate flipTiles (removeDuplicates $ map (normalize . splitDirections) $ lines test)

  putStrLn "== Part 2 =="
  print $ map length $ take 101 $ iterate flipTiles (removeDuplicates $ map (normalize . splitDirections) $ lines input)

splitDirections :: String -> [Direction]
splitDirections []             = []
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

-- Iteratively remove loops and shorten paths. Tweak iterations if required. Sort the result
normalize :: [Direction] -> [Direction]
normalize directions = sort $ last $ take 6 $ iterate (shorten . removeLoops) directions

-- For optimalisation: do only a single iteration
singleNormalize :: [Direction] -> [Direction]
singleNormalize directions = sort $ shorten $ removeLoops directions

-- Try to remove one loop of each type
removeLoops :: [Direction] -> [Direction]
removeLoops directions = foldl
  (\result loop ->
    if all (\d -> elem d result) loop
    then result \\ loop
    else result
  ) directions loops

-- Try to shorten one path of each type
shorten :: [Direction] -> [Direction]
shorten directions = foldl
  (\result (path, replacement) ->
    if all (\d -> elem d result) path
    then replacement:(result \\ path)
    else result
  ) directions shortenings

-- Remove duplicate paths as they flip the same tile twice
removeDuplicates :: [[Direction]] -> [[Direction]]
removeDuplicates paths = map head $ filter (\g -> odd $ length g) $ group $ sort paths

adjacentTiles :: [Direction] -> [[Direction]]
adjacentTiles tile = map singleNormalize [(East:tile),(SouthEast:tile),(SouthWest:tile),(West:tile),(NorthWest:tile),(NorthEast:tile)]

shouldTurnWhite :: [[Direction]] -> [Direction] -> Bool
shouldTurnWhite blackTiles tile = notElem (length $ intersect (adjacentTiles tile) blackTiles) [1,2]

shouldTurnBlack :: [[Direction]] -> [Direction] -> Bool
shouldTurnBlack blackTiles tile = (length $ intersect (adjacentTiles tile) blackTiles) == 2

turnBlackToWhites :: [[Direction]] -> [[Direction]]
turnBlackToWhites blackTiles = blackTiles \\ (filter (shouldTurnWhite blackTiles) blackTiles)

turnWhitesToBlack :: [[Direction]] -> [[Direction]]
turnWhitesToBlack blackTiles =
  let
    whiteAdjacentTiles = (nub $ concat $ map (\tiles -> tiles \\ blackTiles) $ map adjacentTiles blackTiles)
    in (filter (shouldTurnBlack blackTiles) whiteAdjacentTiles)

flipTiles :: [[Direction]] -> [[Direction]]
-- Calculate newBlackTiles and flippedWhiteTiles in parallel, and force the calulcation of both lists before
-- concatenating both lists in the result of this function
flipTiles blackTiles = (force newBlackTiles) `par` (force flippedWhiteTiles) `pseq` newBlackTiles ++ flippedWhiteTiles
  where newBlackTiles = turnBlackToWhites blackTiles
        flippedWhiteTiles = turnWhitesToBlack blackTiles

-- Function copied from http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
-- Forces list to be completely calculated before returning the result
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1
