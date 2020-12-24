import           Data.List
import           Data.List.Split
import           Data.List.Utils
import           Data.Maybe

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ map sides $ parse test
  -- Should all be True
  print $ (\c -> sidesMatch (fromJust $ lookup 1951 c) (fromJust $ lookup 2311 c)) $ map sides $ parse test
  print $ (\c -> sidesMatch (fromJust $ lookup 1427 c) (fromJust $ lookup 2311 c)) $ map sides $ parse test
  print $ (\c -> sidesMatch (fromJust $ lookup 3079 c) (fromJust $ lookup 2311 c)) $ map sides $ parse test

  print $ matchingImages $ map sides $ parse test
  print $ map head $ filter (\c -> length c == 2) $ group $ sort $ flattenPairs $ matchingImages $ map sides $ parse test
  print $ product $ map head $ filter (\c -> length c == 2) $ group $ sort $ flattenPairs $ matchingImages $ map sides $ parse test

  putStrLn "== Part 1 =="
  print $ matchingImages $ map sides $ parse input
  print $ map head $ filter (\c -> length c == 2) $ group $ sort $ flattenPairs $ matchingImages $ map sides $ parse input
  print $ product $ map head $ filter (\c -> length c == 2) $ group $ sort $ flattenPairs $ matchingImages $ map sides $ parse input

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

-- Parse the input to a list of tiles as (tile id, image)
parse :: String -> [(Int, [String])]
parse input =  map (\tile -> (read $ init $ drop 5 $ head tile, tail tile)) $ map lines $ splitOn "\n\n" input

-- Take each side of the image as if it where rotated to be the top row
sides :: (Int, [String]) -> (Int, [String])
sides (id, image) = (id,
  -- Only rotation
  [head image] ++ [map last image] ++ [reverse $ last image] ++ [reverse $ map head image] ++
  -- Rotation plus flip
  [reverse $ head image] ++ [reverse $ map last image] ++ [last image] ++ [map head image]
  )

-- For two tiles, check if at least one side matches
sidesMatch :: [String] -> [String] -> Bool
sidesMatch sides1 sides2 = or [side1 == (reverse side2) | side1 <- sides1, side2 <- sides2]

-- List of (image id, sides) to list of (image id, image id) of images with at least one matching side
matchingImages :: [(Int, [String])] -> [(Int, Int)]
matchingImages images = map (\r -> (fst $ fst r, fst $ snd r)) $ filter (\pair -> sidesMatch (snd $ fst pair) (snd $ snd pair)) $ pairs images

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

flattenPairs :: [(a,a)] -> [a]
flattenPairs pairs = foldr (\(a,b) list -> list ++ [a,b]) [] pairs
