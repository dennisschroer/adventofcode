import           Data.List
import           Data.List.Split
import           Data.List.Utils

main = do
  test <- readFile "test"
  input <- readFile "input"

  putStrLn "== Test Part 1 =="
  print $ part1 test

  putStrLn "== Part 1 =="
  print $ part1 input

  putStrLn "== Test Part 2 =="
  -- Example from textbook. Should be 32688
  print $ part2 $ "x,x,x,x,x,x,x,x,x,x,x,x,x,x,31,x,32,x,33"
  print $ part2 $ "17,x,13,19"
  print $ part2 $ "67,7,59,61"
  print $ part2 $ "67,x,7,59,61"
  print $ part2 $ last $ lines test

  putStrLn "== Part 2 =="

part1 :: String -> Int
part1 input =
  let timestamp         = read $ head $ lines input
      busses            = map read $ filter (/="x") $ splitOn "," $ last $ lines input
      first             = firstBus busses timestamp
      waitTimeForFirst  = waitTime first timestamp
  in first * waitTimeForFirst

waitTime :: Int -> Int -> Int
waitTime busId timestamp = busId - (timestamp `mod` busId)

firstBus:: [Int] -> Int -> Int
firstBus busses timestamp = minimumBy (\a b -> compare (waitTime a timestamp) (waitTime b timestamp)) busses

-- Given two primes p and q, this function calculates with which factor q should be multiplied
-- to be remainder r ahead of a multiple of p.
-- Basically we search for k ∈ Z and r ∈ Zp where r = q^k ∈ Zp
factor :: Int -> Int -> Int -> Int
factor p q r = head [k | k <-[0..], ((q*k) `mod` p) == r]

-- Find the inverse m^-1 of m in Z_p. This is where m^-1 * m == 1 in Z_p
inverse :: Int -> Int -> Int
inverse m p = head [k | k <-[0..p], ((m*k) `mod` p) == 1]

--part2 :: String -> [(Int, Int, Int)]
part2 input =
  let rawBusses             = splitOn "," input
      -- Busses with the required remainder
      -- Each tuple i contains (a_i, m_i) where a_i is the required remainder and m_i is prime
      busses                = map (\(a,b) -> (a, (read::String->Int) b)) $ filter (\x -> snd x /= "x") $ zip [0..length rawBusses] rawBusses
      -- Now we apply the Chinese Remainder Theorem
      -- m is the product of all m_i
      m                     = product $ map snd $ busses
      -- For each m_i, define M_i as m/m_i. Now find x_i = [M_i]^-1 in Z_mi.
      busFactors            = map (\(offset, busId) -> (offset, busId, inverse (m `div` busId) busId)) busses
      -- Now comes the magic. Calculate a_i * M_i * x_i in Z_m
      solutions             = map (\(offset, busId, inverse) -> ((offset * (m `div` busId) * inverse)) `mod` m) busFactors
      -- And calculate the sum in Z_m. This is the solution
      solution              = (sum solutions) `mod` m



      -- The first bus id is the main prime
      -- firstBusId            = snd $ head busses
      -- -- For each bus, calculate how often the bus should be multiplied to have offset a to a mulitple of the first bus
      -- busFactors            = map (\(offset, busId) -> (offset, busId, factor firstBusId busId offset)) busses
      -- -- Using this, calculate for each bus possible departuretimes of the first bus
      -- possibleBusDepartures = map (\(offset, busId, factor) -> [(n * firstBusId + factor) * busId - offset| n <- [1..10000]]) busFactors
  in solution
