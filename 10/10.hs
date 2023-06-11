import Data.List.Split

main = do
  input <- readFile "input"
  let instructions =
        concat
          [ case splitAt 4 l of
              ("noop", _) -> [0]
              ("addx", _ : x) -> [0, read x]
            | l <- lines input
          ]
  let xvalue = 1 : [x + i | (x, i) <- zip xvalue instructions]
  print $ sum [signal * c | (signal, c) <- zip xvalue [1 ..], (c + 20) `mod` 40 == 0]

  let crt =
        [ if abs (pos - pixel `mod` 40) <= 1
            then '#'
            else '.'
          | (pos, pixel) <- zip xvalue [0 ..]
        ]
  putStr $ unlines (chunksOf 40 crt)