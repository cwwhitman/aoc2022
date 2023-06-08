import Control.Applicative
import Data.Char

getStrategy = do s <- getLine
                 if null s then
                    return []
                 else do
                    es <- getStrategy
                    return ((head s, last s) : es)
score (o, p) = result o p + bonus p

-- overengineering <3
result :: Char -> Char -> Int
result o p = 3 * ((ord p - ord o - 1) `mod` 3)

bonus p = ord p - ord 'W'

expected = sum . map score <$> getStrategy

shape o r = chr(ord 'X' + ((ord o + ord r - 1) `mod` 3))

expected2 = sum . map (\(o, r) -> score (o, shape o r)) <$> getStrategy