import Data.Boolean
import Data.List
import Data.List.Split

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
    firstCharEq = ((password passwordLine) !! (minOccurences passwordLine - 1)) == (character passwordLine)
    lastCharEq = ((password passwordLine) !! (maxOccurences passwordLine - 1)) == (character passwordLine)
  in (firstCharEq || lastCharEq) && not (firstCharEq && lastCharEq)

main = do
  input <- readFile "input"
  let passwordLines = map parseLine $ lines input

  putStrLn "== Part 1 =="
  print $ length (filter isValidPassword1 passwordLines)

  -- Oneliner
  -- (p!!4) = password
  -- (p!!2!!0) = character
  -- (read$p!!0) = first number as integer
  -- (read$p!!1) = second number as integer
  print $ length $ filter ((\p -> elem (length $ elemIndices (p!!2!!0) (p!!4)) [(read$p!!0)..(read$p!!1)] ) . splitOneOf "-: ") $ lines input

  putStrLn "== Part 2 =="
  print $ length (filter isValidPassword2 passwordLines)

  -- Oneliner
  print $ length $ filter ((\p -> (1==) $ length $ elemIndices (p!!2!!0) [(p!!4)!!((read$p!!0)-1), (p!!4)!!((read$p!!1)-1)]) . splitOneOf "-: ") $ lines input
