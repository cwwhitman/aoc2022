import Data.List

cascade :: Int -> [a] -> [[a]]
cascade n = take n . iterate (drop 1)

views n xs = takeWhile ((n ==) . length) $ transpose $ cascade n xs

unique _    []     = True
unique seen (x:xs) = x `notElem` seen && unique (x:seen) xs

firstUniq :: Int -> [Char] -> Int
firstUniq n xs = head [i+n | (v, i) <- zip (views n xs) [0..], unique "" v]