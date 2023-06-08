import Data.Char
import Data.List
import Data.List.Split

everynth :: Int -> [a] -> [a]
everynth n [] = []
everynth n (x : xs) = x : everynth n (drop (n - 1) xs)

stacks = do
  s <- getLine
  let row = everynth 4 $ tail s
  if isDigit $ head row
    then return []
    else do
      st <- stacks
      return (row : st)

instructions :: IO [[Int]]
instructions = do
  i <- getLine
  let inst = everynth 2 $ tail $ splitOn " " i
  if null inst
    then return []
    else do
      is <- instructions
      return (map read inst : is)

readInput :: IO ([[Char]], [[Int]])
readInput = do
  st <- stacks
  getLine
  is <- instructions
  return (map (dropWhile (== ' ')) $ transpose st, is)

move sts n s d =
  [ if i == s
      then drop n st
    else if i == d
      -- then reverse (take n (sts !! (s - 1))) ++ st
      then take n (sts !! (s - 1)) ++ st
    else st
    | (st, i) <- zip sts [1 ..]
  ]

applymoves :: [[Char]] -> [[Int]] -> [[Char]]
applymoves = foldl (\st [n, s, d] -> move st n s d)

safehead [] = ' '
safehead xs = head xs

-- one = map safehead . uncurry applymoves <$> readInput
two = map safehead . uncurry applymoves <$> readInput