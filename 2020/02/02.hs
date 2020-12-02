import Data.Boolean

data PasswordLine = PasswordLine { minOccurences :: Int, maxOccurences :: Int, character :: Char, password :: String } deriving (Show)

parseLine :: String -> PasswordLine
parseLine line =
  let
    repl ':' = ' '
    repl '-' = ' '
    repl  c   = c
    [mininumVal, maximumVal, characterVal, passwordVal] = words $ map repl line
  in PasswordLine {minOccurences = read mininumVal, maxOccurences = read maximumVal, character = characterVal !! 0, password = passwordVal}

isValidPassword1 :: PasswordLine -> Bool
isValidPassword1 passwordLine =
  let
    count = length $ filter (\char -> char == (character passwordLine)) (password passwordLine)
  in count >= (minOccurences passwordLine) && count <= (maxOccurences passwordLine)

isValidPassword2 :: PasswordLine -> Bool
isValidPassword2 passwordLine =
  let
    substring = drop (minOccurences passwordLine - 1) $ take (maxOccurences passwordLine) (password passwordLine)
    firstCharEq = (head substring) == (character passwordLine)
    lastCharEq = (last substring) == (character passwordLine)
  in (firstCharEq || lastCharEq) && not (firstCharEq && lastCharEq)

main = do
  contents <- readFile "input"
  let passwordLines = map parseLine $ lines contents

  print $ length (filter isValidPassword1 passwordLines)
  print $ length (filter isValidPassword2 passwordLines)
