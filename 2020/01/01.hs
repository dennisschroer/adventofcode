import System.IO
import Data.List

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

triplets :: [a] -> [(a, a, a)]
triplets l = [(x,y,z) | (x:y:zs) <- tails l, z <- zs]

pairSum :: (Int, Int) -> Int
pairSum (x, y) =  x + y

sumIs2020 :: (Int, Int) -> Bool
sumIs2020 (x, y) = pairSum (x,y) == 2020

tripleSum :: (Int, Int, Int) -> Int
tripleSum (x, y, z) = x + y + z

sumIs2020Triple :: (Int, Int, Int) -> Bool
sumIs2020Triple (x, y, z) = tripleSum (x, y, z) == 2020

displayResult :: Maybe (Int, Int) -> String
displayResult p = maybe "There was no result" (\(x,y) -> ("The result is " ++ (show (x*y)))) p

main = do
  contents <- readFile "input"
  let numbers = sort $ map (read::String->Int) (lines contents)
  let pair = find sumIs2020 $ pairs numbers

  print pair
  print (displayResult pair)

  print $ show $ map tripleSum $ triplets numbers

  let triplet = find sumIs2020Triple $ triplets numbers

  print triplet
