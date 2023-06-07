import Control.Applicative
import Data.List

elves :: IO [Int]
elves = do e <- many readLn
           if null e then
            return []
           else do
            es <- elves
            return (sum e : es)

maxelves :: IO Int
maxelves = maximum <$> elves

topelves n = sum . take n . reverse . sort <$> elves