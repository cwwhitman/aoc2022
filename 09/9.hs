import Data.List

simulate :: (Int, Int) -> (Int, Int) -> (Int, Int)
simulate (hx, hy) (tx, ty) =
  if (tx, ty) `elem` aroundh
    then (tx, ty)
    else head (aroundh `intersect` aroundt)
  where aroundh = [(hx + dx, hy + dy) | 
                   (xs, ys) <- zip [[0],[-1,1],[0],[-1,1]] 
                                   [[0],[0],[-1,1],[-1,1]],
                   dx <- xs,
                   dy <- ys
                  ]
        aroundt = [(tx + dx, ty + dy) | dx <- [(-1) .. 1], dy <- [(-1) .. 1]]

update (hx, hy) dir
    | dir == 'U' = (hx, hy + 1)
    | dir == 'D' = (hx, hy - 1)
    | dir == 'L' = (hx - 1, hy)
    | dir == 'R' = (hx + 1, hy)

count :: [(Int, Int)] -> Int
count = length . group . sort

states n = foldl
          (\(snake:ss) dir ->
            let nh = update (head snake) dir in
            reverse (foldl (\front seg -> simulate (head front) seg : front ) [nh] (tail snake))
          : (snake:ss))
          [replicate n (0, 0)]

main = do
  input <- readFile "input"
  let instructions = map (\(dir : _ : d) -> (dir, read d)) (lines input)
  let snake2 = states 2 [dir | (dir, n) <- instructions, _ <- [1 .. n]]
  let snake10 = states 10 [dir | (dir, n) <- instructions, _ <- [1 .. n]]
  print $ count (map last snake2)
  print $ count (map last snake10)