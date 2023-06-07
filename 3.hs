halve xs = splitAt n xs
    where n = length xs `div` 2

getBags :: IO [(String, String)]
getBags = do b <- getLine
             if null b then
              return []
             else do
              bs <- getBags
              return (halve b : bs)

