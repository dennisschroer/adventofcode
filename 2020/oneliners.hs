-- Single file in which all oneliners are collected
-- For breakdown of these oneliners view the solutions for each day

import Data.List
import Data.List.Split

main = do
  putStrLn "Day 1 Part 1"
  readFile "01/input" >>= (\i -> print $ head $ (\l -> [x*y | x<-l, y<-l, x+y==2020]) $ map (read::String->Int) $ lines i)

  putStrLn "Day 1 Part 2"
  readFile "01/input" >>= (\i -> print $ head $ (\l -> [x*y*z | x<-l, y<-l, z<-l, x+y+z==2020]) $ map (read::String->Int) $ lines i)

  putStrLn "Day 5 Part 1"
  readFile "05/input" >>= (\i -> print $ maximum $ map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines i)

  putStrLn "Day 5 Part 2"
  readFile "05/input" >>= (\i -> print $ [54..930] \\ (map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines i))

  putStrLn "Day 6 Part 1"
  readFile "06/input" >>= (\i -> print $ sum $ map (length . (foldl union "") . lines) $ splitOn "\n\n" i)

  putStrLn "Day 6 Part 2"
  readFile "06/input" >>= (\i -> print $ sum $ map (length . (foldl intersect ['a'..'z']) . lines) $ splitOn "\n\n" i)
