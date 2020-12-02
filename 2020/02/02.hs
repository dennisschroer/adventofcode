data PasswordLine = PasswordLine { minOccurences :: Int, maxOccurences :: Int, character :: Char, password :: String } deriving (Show)

parseLine :: String -> PasswordLine
parseLine line =
  let
    repl ':' = ' '
    repl '-' = ' '
    repl  c   = c
    [mininumVal, maximumVal, characterVal, passwordVal] = words $ map repl line
  in PasswordLine {minOccurences = read mininumVal, maxOccurences = read maximumVal, character = characterVal !! 0, password = passwordVal}

isValidPassword :: PasswordLine -> Bool
isValidPassword passwordLine =
  let
    count = length $ filter (\char -> char == (character passwordLine)) (password passwordLine)
  in count >= (minOccurences passwordLine) && count <= (maxOccurences passwordLine)

main = do
  contents <- readFile "input"
  let passwordLines = map parseLine $ lines contents

  print $ length (filter isValidPassword passwordLines)
