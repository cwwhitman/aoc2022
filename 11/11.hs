import Data.Char (isSpace)
import Data.List.Split
import Data.List

parseEq (l : o : r : _) n = left `op` right
  where
    left
      | l == "old" = n
      | otherwise = read l
    right
      | r == "old" = n
      | otherwise = read r
    op
      | o == "*" = (*)
      | o == "+" = (+)
      | otherwise = error "invalid operator"

type Inv = (Int, [Int])
type Monkey = (Int -> Int, Int, Int, Int)

parseInput :: String -> [(Inv, Monkey)]
parseInput input = map parseMonkey monkeys
  where
    monkeys = splitWhen null . lines $ input
    nm = length monkeys
    parseMonkey (i' : inv : op : test : tm : fm : _) =
      ( (0, map (read . filter (not . isSpace)) . splitOn "," . tail $ dropWhile (/= ':') inv),
        (
          parseEq . words . tail $ dropWhile (/= '=') op,
          read . last . words $ test,
          ((read . last . words $ tm) - i - 1) `mod` nm,
          ((read . last . words $ fm) - i - 1) `mod` nm
        )
      )
      where i = read . init . last . words $ i'
    parseMonkey _ = error "bad monkey :("

assign :: (a -> Bool) -> [a] -> ([a], [a])
assign = assign' [] []
  where
    assign' ts fs _ [] = (ts, fs)
    assign' ts fs g (x : xs)
      | g x = assign' (x : ts) fs g xs
      | otherwise = assign' ts (x : fs) g xs

turn :: Bool -> Int -> Monkey -> [Inv] -> [Inv]
turn anx ring (f, test, g, b) store = zip (as ++ [a + length inv]) (zipWith (++) invs
  [if i == g then good else if i == b then bad else [] | i <- [0..]] ++ [[]])
  where
    (a:as, inv:invs) = unzip store
    (good, bad) = assign ((0 ==) . (`mod` test)) . map ((if anx then (`mod`ring) else (`div`3)) . f) . reverse $ inv

mround :: Bool -> [Monkey] -> [Inv] -> [Inv]
mround anx ms invs = foldl' (\x f -> f x) invs $ map (turn anx ring) ms
  where ring = product $ map (\(_, t, _, _) -> t) ms

rounds 0 anx _ items = items
rounds n anx ms items = rounds (n-1) anx ms (mround anx ms items)

main = do
  input <- readFile "input"
  let (items, monkeys) = unzip . parseInput $ input
  -- let (_, _, a, b) = (head monkeys)
  -- print (a, b)

  -- print $ foldl1 (zipWith (+)) . map (map length). take 20 . iterate (mround monkeys) $ items
  print $ product . take 2 . sortBy (flip compare) . map fst $ rounds 20 False monkeys items
  print $ product . take 2 . sortBy (flip compare) . map fst $ rounds 10000 True monkeys items


