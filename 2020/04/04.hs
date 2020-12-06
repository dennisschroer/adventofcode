import Data.List.Split
import Data.List
import Data.Maybe
import Data.Char
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

  -- Part one, single line
  print $ length $ fst $ partition (7==) $ map (length . delete "cid" . nub . map (head . splitOn ":") . delete "" . splitOneOf "\n ") $ splitOn "\n\n" input

  -- Even shorter
  print $ length $ filter (7==) $ map (length . delete "cid" . map (head . splitOn ":") . words) $ splitOn "\n\n" input

  -- Part two
  let passports = map (delete "") $ map (splitOneOf "\n ") $ splitOn "\n\n" input
  let passportFields = map (map (splitOn ":")) passports
  let passportMaps = map (foldl (\passportMap fields -> Map.insert (fields !! 0) (fields!!1) passportMap) Map.empty ) passportFields

  let validPassports = filter isByrValid $ filter isIyrValid $ filter isEyrValid $ filter isHgtValid $ filter isHclValid $ filter isEclValid $ filter isPidValid passportMaps
  print $ length validPassports

  -- Oneliner with some explanation
  -- Print the length of all valid passports
  print $ length $
    -- Reduce the list of passports to only valid passports
    foldl
    -- Reducer function : apply each check as a filter
    (\passports check -> filter check passports)
    -- Initial value of reducer is the list of passports. Type is [(String,String)]
    -- Example: [[("hgt","64in"),("ecl","oth"),("hcl","#18171d"),("pid","105602506"),("byr","1973"),("eyr","2022"),("iyr","2014")]]
    (map (map (\l -> let s = (splitOn ":" l) in (s!!0,s!!1)) . words) $ splitOn "\n\n" input)
    -- List of checks to reduce the set of passports
    -- Each check has type [(String, String)] -> Bool
    [
      (\p -> elem (maybe 0 read $ lookup "byr" p) [1920..2002]),
      (\p -> elem (maybe 0 read $ lookup "iyr" p) [2010..2020]),
      (\p -> elem (maybe 0 read $ lookup "eyr" p) [2020..2030]),
      (\p -> let (v,u) = span isDigit $ fromMaybe "0cm" $ lookup "hgt" p in or [u == "cm" && elem (read v) [150..193], u == "in" && elem (read v) [59..76]]),
      (\p -> let x = (fromMaybe "#" $ lookup "hcl" p) in head x == '#' && length x == 7 && (all isHexDigit $ tail x)),
      (\p -> elem (fromMaybe "" $ lookup "ecl" p) ["amb","blu","brn","gry","grn","hzl","oth"]),
      (\p -> maybe False (\x -> length x == 9 && read x > 0) $ lookup "pid" p)
    ]


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
