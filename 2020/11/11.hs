import           Data.List
import           Data.List.Utils
import           Data.Maybe

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  printState $ iterateWhileChanging $ parse test
  print $ sum $ map length $ map (filter (=='#')) $ iterateWhileChanging $ parse test

  putStrLn "== Part 1 =="
  let finalState = iterateWhileChanging $ parse input
  printState finalState
  print $ sum $ map length $ map (filter (=='#')) finalState

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

parse :: String -> [String]
parse input = lines input

printState :: [String] -> IO()
printState state = mapM_ putStrLn state

iterateWhileChanging :: [String] -> [String]
iterateWhileChanging beginState = last $ unfoldr (\state -> let nextState = iterateSeats state in if state == nextState then Nothing else Just (nextState, nextState)) beginState

iterateSeats :: [String] -> [String]
iterateSeats state =
  let
    height = length state
    width = length (head state)
    in [[nextState (surroundingSeats state x y) (state!!y!!x) | x <- [0..width-1]] | y <- [0..height-1] ]

nextState :: String -> Char -> Char
nextState surroundings currentState
  | currentState == '.'                           = '.'
  | (length $ filter (=='#') surroundings) > 3    = 'L'
  | (length $ filter (=='#') surroundings) == 0   = '#'
  | otherwise                                     = currentState

surroundingSeats :: [String] -> Int -> Int -> String
surroundingSeats state x y = catMaybes $ map (\f -> f state x y) [upLeft, up, upRight, left, right, bottomLeft, bottom, bottomRight]

upLeft :: [String] -> Int -> Int -> Maybe Char
upLeft state x y
  | x == 0 || y == 0                                          = Nothing
  | otherwise                                                 = Just (state !! (y-1) !! (x-1))

up :: [String] -> Int -> Int -> Maybe Char
up state x y
  | y == 0                                                    = Nothing
  | otherwise                                                 = Just (state !! (y-1) !! x)

upRight :: [String] -> Int -> Int -> Maybe Char
upRight state x y
  | x == (length (head state) - 1) || y == 0                  = Nothing
  | otherwise                                                 = Just (state !! (y-1) !! (x+1))

left :: [String] -> Int -> Int -> Maybe Char
left state x y
  | x == 0                                                    = Nothing
  | otherwise                                                 = Just (state !! y !! (x-1))

right :: [String] -> Int -> Int -> Maybe Char
right state x y
  | x == (length (head state) - 1)                            = Nothing
  | otherwise                                                 = Just (state !! y !! (x+1))

bottomLeft :: [String] -> Int -> Int -> Maybe Char
bottomLeft state x y
  | x == 0 || y == (length state - 1)                         = Nothing
  | otherwise                                                 = Just (state !! (y+1) !! (x-1))

bottom :: [String] -> Int -> Int -> Maybe Char
bottom state x y
  | y == (length state - 1)                                   = Nothing
  | otherwise                                                 = Just (state !! (y+1) !! x)

bottomRight :: [String] -> Int -> Int -> Maybe Char
bottomRight state x y
  | x == (length (head state) - 1) || y == (length state - 1) = Nothing
  | otherwise                                                 = Just (state !! (y+1) !! (x+1))
