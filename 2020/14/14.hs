import           Data.List
import           Data.List.Utils
import Data.List.Split
import qualified Data.Map        as Map
import Data.Bits

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ Map.foldr (+) 0 $ fst $ runProgram (Map.empty ,"") $ lines test

  putStrLn "== Part 1 =="
  print $  Map.foldr (+) 0 $ fst $ runProgram (Map.empty ,"") $ lines input

  putStrLn "== Test Part 2 =="

  putStrLn "== Part 2 =="

runProgram :: (Map.Map Int Int, String) -> [String] -> (Map.Map Int Int, String)
runProgram state [] = state
runProgram (memory, mask) (instruction:instructions) =
  let
    parts = splitOn " = " instruction
    command = head parts
    argument = last parts
    in case command of
      "mask" -> runProgram (changeMask (memory, mask) argument) instructions
      otherwise -> runProgram (writeMemory (memory, mask) (read $ init $ drop 4 command) (read argument)) instructions


changeMask :: (Map.Map Int Int, String) -> String -> (Map.Map Int Int, String)
changeMask (memory, mask) newMask = (memory, newMask)

writeMemory :: (Map.Map Int Int, String) -> Int -> Int -> (Map.Map Int Int, String)
writeMemory (memory, mask) address value = (Map.insert address (maskValue mask value) memory, mask)

maskValue :: String -> Int -> Int
maskValue mask value =
  let
    -- Ones mask is used with bitwise or to force all ones to be one in the output
    onesMask = sum $ map (2^) $ elemIndices '1' $ reverse mask
    -- Zeros mask is used with bitwise and to force all zeroes to be zero in the output
    zerosMask = sum $ map (2^) $ elemIndices '1' $ replace "X" "1" $ reverse mask
    in (value .&. zerosMask) .|. onesMask
