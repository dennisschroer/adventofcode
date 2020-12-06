-- Single file in which all oneliners are collected
-- For breakdown of these oneliners view the solutions for each day

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

main = do
  putStrLn "Day 1 Part 1 (expense sum 2020)"
  readFile "01/input" >>= (\i -> print $ head $ (\l -> [x*y | x<-l, y<-l, x+y==2020]) $ map (read::String->Int) $ lines i)

  putStrLn "Day 1 Part 2 (expense sum 2020)"
  readFile "01/input" >>= (\i -> print $ head $ (\l -> [x*y*z | x<-l, y<-l, z<-l, x+y+z==2020]) $ map (read::String->Int) $ lines i)

  putStrLn "Day 2 Part 1 (password checks)"
  readFile "02/input" >>= (\i -> print $ length $ filter ((\p -> elem (length $ elemIndices (p!!2!!0) (p!!4)) [(read$p!!0)..(read$p!!1)] ) . splitOneOf "-: ") $ lines i)

  putStrLn "Day 2 Part 2 (password checks)"
  readFile "02/input" >>= (\i -> print $ length $ filter ((\p -> (1==) $ length $ elemIndices (p!!2!!0) [(p!!4)!!((read$p!!0)-1), (p!!4)!!((read$p!!1)-1)]) . splitOneOf "-: ") $ lines i)

  putStrLn "Day 3 Part 1 (toboggan trees)"
  readFile "03/input" >>= (\i -> print $ length $ filter ('#'==) $ [ lines i !! j !! (mod (j*3) $ length $ head $ lines i) | j <- [0..((length $ lines i) - 1)]])

  putStrLn "Day 3 Part 2 (toboggan trees)"
  readFile "03/input" >>= (\i -> print $ foldl (*) 1 $ map (\(r,d) -> length $ filter ('#'==) $ [ lines i !! (j*d) !! (mod (j*r) (length $ head $ lines i)) | j <- [0..(((length $ lines i) `div` d) - 1)]]) [(1,1), (3,1), (5,1), (7,1), (1,2)])

  putStrLn "Day 4 Part 1 (passport validation)"
  readFile "04/input" >>= (\i -> print $ length $ filter (7==) $ map (length . delete "cid" . map (head . splitOn ":") . words) $ splitOn "\n\n" i)

  putStrLn "Day 4 Part 2 (passport validation)"
  readFile "04/input" >>= (\i -> print $ length $ foldl (\passports check -> filter check passports) (map (map (\l -> let s = (splitOn ":" l) in (s!!0,s!!1)) . words) $ splitOn "\n\n" i) [(\p -> elem (maybe 0 read $ lookup "byr" p) [1920..2002]), (\p -> elem (maybe 0 read $ lookup "iyr" p) [2010..2020]), (\p -> elem (maybe 0 read $ lookup "eyr" p) [2020..2030]), (\p -> let (v,u) = span isDigit $ fromMaybe "0cm" $ lookup "hgt" p in or [u == "cm" && elem (read v) [150..193], u == "in" && elem (read v) [59..76]]), (\p -> let x = (fromMaybe "#" $ lookup "hcl" p) in head x == '#' && length x == 7 && (all isHexDigit $ tail x)), (\p -> elem (fromMaybe "" $ lookup "ecl" p) ["amb","blu","brn","gry","grn","hzl","oth"]), (\p -> maybe False (\x -> length x == 9 && read x > 0) $ lookup "pid" p)])

  putStrLn "Day 5 Part 1 (plane boarding)"
  readFile "05/input" >>= (\i -> print $ maximum $ map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines i)

  putStrLn "Day 5 Part 2 (plane boarding)"
  readFile "05/input" >>= (\i -> print $ [54..930] \\ (map (sum . map (2^) . findIndices (\c -> elem c "BR") . reverse) $ lines i))

  putStrLn "Day 6 Part 1 (custom declaration)"
  readFile "06/input" >>= (\i -> print $ sum $ map (length . (foldl union "") . lines) $ splitOn "\n\n" i)

  putStrLn "Day 6 Part 2 (custom declaration)"
  readFile "06/input" >>= (\i -> print $ sum $ map (length . (foldl intersect ['a'..'z']) . lines) $ splitOn "\n\n" i)
