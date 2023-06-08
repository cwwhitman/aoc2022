views n xs = takeWhile ((n ==) . length) $ map (take n) $ iterate tail xs

unique _    []     = True
unique seen (x:xs) = x `notElem` seen && unique (x:seen) xs

firstUniq :: Int -> [Char] -> Int
firstUniq n xs = head [i+n | (v, i) <- zip (views n xs) [0..], unique "" v]