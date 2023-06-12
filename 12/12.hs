import Data.Char
import Data.List

parseInput :: String -> ((Int, Int), (Int, Int), [(Int, Int)], [[Int]])
parseInput input =
  let (starts, ends, lows, grid) =
        unzip4 [unzip4 [height x y h | (x, h) <- zip [0 ..] line] | (y, line) <- zip [0 ..] (lines input)]
   in (getpos starts, getpos ends, concat . concat $ lows, grid)
  where
    height x y h
      | h == 'S' = ([(x, y)],[], [], 0)
      | h == 'E' = ([], [], [(x, y)], 25)
      | h == 'a' = ([], [], [(x, y)], 0)
      | otherwise = ([], [], [], ord h - ord 'a')
    getpos = head . concat . concat

getheight grid (x, y)
    | 0 <= x && x < width &&
      0 <= y && y < height = grid !! y !! x
    | otherwise = 100
    where (height, width) = (length grid, length (head grid))


connected :: [[Int]] -> (Int, Int) -> [(Int, Int)]
connected grid (x, y) = [(x+dx, y+dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                getheight grid (x+dx, y+dy) - h <= 1]
    where h = getheight grid (x, y)

bfs' :: [[Int]] -> [(Int, Int)] -> [(Int, (Int, Int))] -> (Int, Int) -> Int
bfs' _ _ [] _ = -1
bfs' grid visited srcs dst =
  if not . null $ reached
    then fst . head $ reached
    else bfs' grid visited' adjacent dst
  where
    reached = filter ((dst ==) . snd) srcs
    (visited', adjacent) =
      foldl'
        ( \(v, adjs) (dst, src) ->
            let adj = filter (`notElem` v) (connected grid src)
             in (adj ++ v, map (dst + 1,) adj ++ adjs)
        )
        (visited, [])
        srcs

bfs grid src = bfs' grid [src] [(0, src)]

main = do
    input <- readFile "input"
    let (start, end, lows, grid) = parseInput input

    print (start, end)
    putStr (unlines $ map show grid)
    print $ bfs grid start end

    let lowsdist = filter (/= (-1)) $ map (\s -> bfs grid s end) (start:lows)

    print $ minimum lowsdist

