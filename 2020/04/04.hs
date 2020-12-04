import Data.List.Split
import Data.List

main = do
  input <- readFile "input"

  -- Part one
  let passports = map (splitOneOf "\n ") $ splitOn "\n\n" input
  let passportKeys = map (map (head . (splitOn ":"))) passports
  let uniquePassportKeys = map nub passportKeys
  let uniquePasswortKeysWithoutCid = map (delete "") $ map (delete "cid") uniquePassportKeys
  let keyCounts = map length uniquePasswortKeysWithoutCid
  let numberOfValids = length $ fst $ partition (7==) keyCounts

  print numberOfValids
