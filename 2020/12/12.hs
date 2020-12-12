import           Data.List
import           Data.List.Utils
import           Data.Maybe

main = do
  test <- readFile "test"
  test_extended <- readFile "test_extended"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ navigate (0, 0, 90) $ lines test
  print $ (\(x,y,_) -> manhattan x y ) $ navigate (0, 0, 90) $ lines test

  putStrLn "== Part 1 =="
  print $ navigate (0, 0, 90) $ lines input
  print $ (\(x,y,_) -> manhattan x y) $ navigate (0, 0, 90) $ lines input

  putStrLn "== Test Part 2 =="
  -- Output: (214,-72,4,-10)
  print $ navigateWaypoint (0, 0, 10, 1) $ lines test
  -- Output: 286
  print $ (\(x,y,_,_) -> manhattan x y) $ navigateWaypoint (0, 0, 10, 1) $ lines test

  -- Output: (214,-72,4,-10)
  print $ navigateWaypoint (0, 0, 10, 1) $ lines test_extended
  -- Output: 286
  print $ (\(x,y,_,_) -> manhattan x y) $ navigateWaypoint (0, 0, 10, 1) $ lines test_extended

  putStrLn "== Part 2 =="
  -- Output: (-2470,8091,-67,9)
  print $ navigateWaypoint (0, 0, 10, 1) $ lines input
  -- Output: 10561
  print $ (\(x,y,_,_) -> manhattan x y) $ navigateWaypoint (0, 0, 10, 1) $ lines input


navigate :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
-- Base case: no commands left
navigate (x, y, direction) [] = (x, y, direction)
-- Recursive case: at lease one command left
navigate (x, y, direction) (command:commands) =
  let action = head command
      value  = read $ tail command
  in case action of
    'N' -> navigate (x, y+value, direction) commands
    'S' -> navigate (x, y-value, direction) commands
    'E' -> navigate (x+value, y, direction) commands
    'W' -> navigate (x-value, y, direction) commands
    'L' -> navigate (x, y, mod (direction - value) 360) commands
    'R' -> navigate (x, y, mod (direction + value) 360) commands
    'F' -> navigate (fromJust $ lookup direction [(0, x), (90, x + value), (180, x), (270, x - value)], fromJust $ lookup direction [(0, y + value), (90, y), (180, y - value), (270, y)], direction) commands

manhattan :: Int -> Int -> Int
manhattan x y = (abs x)+(abs y)

navigateWaypoint :: (Int, Int, Int, Int) -> [String] -> (Int, Int, Int, Int)
-- Base case: no commands left
navigateWaypoint (x, y, wx, wy) [] = (x, y, wx, wy)
-- Recursive case: at lease one command left
-- x, y = ships current position. Positive=east/north
-- wx, xy = Position of waypoint relative to ship. Positive=east/north
navigateWaypoint (x, y, wx, wy) (command:commands) =
  let action = head command
      value  = read $ tail command
  in case action of
    'N' -> navigateWaypoint (x, y, wx, wy+value) commands
    'S' -> navigateWaypoint (x, y, wx, wy-value) commands
    'E' -> navigateWaypoint (x, y, wx+value, wy) commands
    'W' -> navigateWaypoint (x, y, wx-value, wy) commands
    'L' -> navigateWaypoint (x, y, -wy, wx) commands
    'R' -> navigateWaypoint (x, y, wy, -wx) commands
    'F' -> navigateWaypoint (x + value * wx, y + value * wy, wx, wy) commands
