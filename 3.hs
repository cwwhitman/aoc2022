import Data.Char
halve xs = [take n xs, drop n xs]
  where
    n = length xs `div` 2

getBags :: IO [String]
getBags = do
  b <- getLine
  if null b
    then return []
    else do
      bs <- getBags
      return (b : bs)

findcommon :: [String] -> String
findcommon [xs] = xs
findcommon (xs:ys:ss) = findcommon $ filter (`elem` xs) ys : ss

priority c | c `elem` ['a'..'z'] = ord c - ord 'a' + 1
           | otherwise           = ord c - ord 'A' + 27

one = sum . map (priority . head . findcommon . halve) <$> getBags

split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = take n xs : split n (drop n xs)

two = sum . map (priority . head . findcommon) . split 3 <$> getBags