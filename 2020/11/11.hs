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
  -- let finalState = iterateWhileChanging $ parse input
  -- printState finalState
  -- print $ sum $ map length $ map (filter (=='#')) finalState

  putStrLn "== Test Part 2 =="
  print $ firstSeatInDirection [".L.L.#.#.#.#."] (1,0) ((+1), id)
  print $ visibleSeats (parse test) (1,1)

  printState $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ parse test
  putStrLn "========================"
  printState $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ iterateSeats2 $ parse test

  let finalState = iterateWhileChanging2 $ parse test
  printState finalState
  print $ sum $ map length $ map (filter (=='#')) finalState

  putStrLn "== Part 2 =="
  let finalState = iterateWhileChanging2 $ parse input
  printState finalState
  print $ sum $ map length $ map (filter (=='#')) finalState

parse :: String -> [String]
parse input = lines input

printState :: [String] -> IO()
printState state = mapM_ putStrLn state

-- Functions for part 2
getAt :: [String] -> (Int, Int) -> Maybe Char
getAt state (x, y)
  -- Coordinates are out of range
  | x < 0 || y < 0 || x >= (length (head state)) || y >= (length state) = Nothing
  | otherwise                                                           = Just (state !! y !! x)

-- Apply the given projection untill a seat is found or the edge is reached
firstSeatInDirection :: [String] -> (Int, Int) -> (Int->Int, Int->Int) -> Maybe Char
firstSeatInDirection state position (dx, dy) = listToMaybe $ dropWhile (=='.') $ map fromJust $ takeWhile isJust $ map (getAt state) $ drop 1 $ iterate (project (dx, dy)) position

-- For each direction, find the first seat or nothing if there is not seat in sight
visibleSeats :: [String] -> (Int, Int) -> String
visibleSeats state position =
  let u = subtract 1
      d = (+1)
  in catMaybes $ map (firstSeatInDirection state position) [(u, u), (id, u), (d, u), (u, id), (d, id), (u, d), (id, d), (d, d)]

-- Apply the projection to coordinates
project :: (Int -> Int, Int -> Int) -> (Int, Int) -> (Int, Int)
project (dx, dy) (x,y) = (dx x, dy y)

nextState2 :: String -> Char -> Char
nextState2 surroundings currentState
  | currentState == '.'                           = '.'
  | (length $ filter (=='#') surroundings) > 4    = 'L'
  | (length $ filter (=='#') surroundings) == 0   = '#'
  | otherwise                                     = currentState

iterateSeats2 :: [String] -> [String]
iterateSeats2 state =
  let
    height = length state
    width = length (head state)
    in [[nextState2 (visibleSeats state (x,y)) (state!!y!!x) | x <- [0..width-1]] | y <- [0..height-1]]

iterateWhileChanging2 :: [String] -> [String]
iterateWhileChanging2 beginState = last $ unfoldr (\state -> let nextState = iterateSeats2 state in if state == nextState then Nothing else Just (nextState, nextState)) beginState

-- Function for part 1
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
