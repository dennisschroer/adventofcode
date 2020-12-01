import System.IO
import Data.List

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

combinations k ns = filter ((k==).length) $ subsequences ns

triplets :: [a] -> [(a, a, a)]
-- triplets l = [(x,y,z) | (x:y:zs) <- tails l, z <- zs]
triplets l = [(x,y,z) | [x,y,z] <- filter ((3==).length) $ subsequences l]

pairSum :: (Int, Int) -> Int
pairSum (x, y) =  x + y

pairProduct :: (Int, Int) -> Int
pairProduct (x, y) =  x * y

sumIs2020 :: (Int, Int) -> Bool
sumIs2020 (x, y) = pairSum (x,y) == 2020

tripleSum :: (Int, Int, Int) -> Int
tripleSum (x, y, z) = x + y + z

tripleProduct :: (Int, Int, Int) -> Int
tripleProduct (x, y, z) = x * y * z

sumIs2020Triple :: (Int, Int, Int) -> Bool
sumIs2020Triple (x, y, z) = tripleSum (x, y, z) == 2020

main = do
  contents <- readFile "input"
  let numbers = sort $ map (read::String->Int) (lines contents)
  let pair = find sumIs2020 $ pairs numbers

  print "part 1"
  print pair
  print (maybe 0 pairProduct pair)

  -- print $ show $ map tripleSum $ triplets numbers

  let triplet = find sumIs2020Triple $ triplets numbers

  print "part 2"
  print triplet
  print (maybe 0 tripleProduct triplet)
