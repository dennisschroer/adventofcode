import           Data.List
import           Data.List.Utils

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  putStrLn "Instructions"
  print $ parse test
  putStrLn "Executions"
  print $ executeStop $ parse test
  putStrLn "Accumulator value"
  print $ acc $ last $ executeStop $ parse test

  putStrLn "== Part 1 =="
  print $ acc $ last $ executeStop $ parse input

  putStrLn "== Test Part 2 =="
  print $ acc $ findBug $ parse test

  putStrLn "== Part 2 =="
  print $ acc $ findBug $ parse input

parse :: String -> [(String, Int)]
parse input = map (\l -> (take 3 l, read $ drop 4 $ replace "+" "" l)) $ lines input

-- State of the processor
-- pc = proces counter, a pointer to the instuction
-- acc = accumulator
-- history = history of executued instructions indicated with process counters
data State = State { pc :: Int, acc :: Int, history :: [Int] } deriving (Show)

-- Execute a single instruction
executeInstruction :: State -> [(String, Int)] -> State
executeInstruction state program =
  let
    counter = pc state
    instruction = program !! counter
    command = fst instruction
    amount = snd instruction
    in case command of
      "acc" -> State {pc = counter + 1, acc = (acc state) + amount, history = (history state) ++ [counter] }
      "jmp" -> State {pc = counter + amount, acc = (acc state), history = (history state) ++ [counter] }
      otherwise -> State {pc = counter + 1, acc = (acc state), history = (history state) ++ [counter] }

-- Execute to infinity
execute :: [(String, Int)] -> [State]
execute program = unfoldr (\s -> let next = executeInstruction s program in Just (s, next)) State {pc=0, acc=0, history=[]}

-- Execute untill loop is detected or end is reached
executeStop :: [(String, Int)] -> [State]
executeStop program = takeWhileInclusive (\s -> (pc s) < (length program) && notElem (pc s) (history s)) $ execute program

-- Replace the command at position i with nop command
replaceCommandWithNop :: [(String, Int)]-> Int -> [(String, Int)]
replaceCommandWithNop program i = take i program ++ [("nop", 0)] ++ drop (i+1) program

-- Repeatedly replace command with nop untill there is no loop. Return end state of this execution
findBug :: [(String, Int)] -> State
findBug program = maximumBy (\a b -> compare (pc a) (pc b)) $ map (last . executeStop . replaceCommandWithNop program) [0..length program]

-- Inclusive variant of default takeWhile function
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
