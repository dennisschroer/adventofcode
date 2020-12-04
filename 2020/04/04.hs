import Data.List.Split
import Data.List
import qualified Data.Map as Map

main = do
  input <- readFile "input"

  -- Part one
  let passports = map (delete "") $ map (splitOneOf "\n ") $ splitOn "\n\n" input
  let passportKeys = map (map (head . (splitOn ":"))) passports
  let uniquePassportKeys = map nub passportKeys
  let uniquePasswortKeysWithoutCid = map (delete "cid") uniquePassportKeys
  let keyCounts = map length uniquePasswortKeysWithoutCid
  let numberOfValids = length $ fst $ partition (7==) keyCounts

  print numberOfValids

  -- Part two
  let passports = map (delete "") $ map (splitOneOf "\n ") $ splitOn "\n\n" input
  let passportFields = map (map (splitOn ":")) passports
  let passportMaps = map (foldl (\passportMap fields -> Map.insert (fields !! 0) (fields!!1) passportMap) Map.empty ) passportFields

  let validPassports = filter isByrValid $ filter isIyrValid $ filter isEyrValid $ filter isHgtValid $ filter isHclValid $ filter isEclValid $ filter isPidValid passportMaps
  print $ length validPassports

isByrValid :: Map.Map String String -> Bool
isByrValid passport = maybe False (\x -> (read x) >= 1920 && (read x) <= 2002) (Map.lookup "byr" passport)

isIyrValid :: Map.Map String String -> Bool
isIyrValid passport = maybe False (\x -> (read x) >= 2010 && (read x) <= 2020) (Map.lookup "iyr" passport)

isEyrValid :: Map.Map String String -> Bool
isEyrValid passport = maybe False (\x -> (read x) >= 2020 && (read x) <= 2030) (Map.lookup "eyr" passport)

isHgtValid :: Map.Map String String -> Bool
isHgtValid passport =
  let
    value = Map.lookup "hgt" passport
    unit = maybe "" (reverse . take 2 . reverse) value
    amount = maybe 0 (read . reverse . drop 2 . reverse) value
  in isHeightValid amount unit

isHeightValid :: Int -> String -> Bool
isHeightValid amount "cm" = amount >= 150 && amount <= 193
isHeightValid amount "in" = amount >= 59 && amount <= 76
isHeightValid _ _ = False

isHclValid :: Map.Map String String -> Bool
isHclValid passport = maybe False (\x -> head x == '#' && (length $ intersect (tail x) "0123456789abcdef") == 6) (Map.lookup "hcl" passport)

isEclValid :: Map.Map String String -> Bool
isEclValid passport = maybe False (\x -> elem x ["amb","blu","brn","gry","grn","hzl","oth"]) (Map.lookup "ecl" passport)

isPidValid :: Map.Map String String -> Bool
isPidValid passport = maybe False (\x -> length x == 9 && read x > 0) (Map.lookup "pid" passport)
