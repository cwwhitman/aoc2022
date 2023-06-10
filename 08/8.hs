import Data.Char
import Data.List

visible _ [] = []
visible m (t : ts) = (m < t) : visible (max m t) ts

distanceSeen _ [] = 0
distanceSeen h (x : xs)
  | x < h = 1 + distanceSeen h xs
  | otherwise = 1

beauty :: [Int] -> [Int] -> [Int]
beauty _ [] = []
beauty l (x : r) = product (map (distanceSeen x) [l, r]) : beauty (x : l) r

main = do
  input <- readFile "input"
  let trees = map (map digitToInt) $ lines input
  let visibilities = [u (map (visible (-1)) (v trees)) | (v, u) <- zip view unview]
        where
          view = [g . f | f <- [id, transpose], g <- [id, map reverse]]
          unview = [f . g | f <- [id, transpose], g <- [id, map reverse]]
  let visibility = foldl1 (zipWith (zipWith (||))) visibilities
  print (sum [1 | v <- concat visibility, v])

  let beauties =
        zipWith
          (zipWith (*))
          (map (beauty []) trees)
          (transpose (map (beauty []) (transpose trees)))
  print (maximum $ concat beauties)
