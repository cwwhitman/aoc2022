import Text.Parsec
import Text.Parsec.String
import Data.List.Split
import Data.List

data Packet = Con Int | List [Packet] deriving (Show, Eq)

natural :: Parser Int
natural = read <$> many1 digit

number :: Parser Int
number = do char '-'
            n <- natural
            return (-n)
        <|> natural

parseList :: Parser [Packet]
parseList = do char '['
               ps <- (do x <- parsePacket
                         xs <- many (do char ','
                                        parsePacket)
                         return (x:xs))
                     <|> return []
               char ']'
               return ps

parsePacket :: Parser Packet
parsePacket = (Con <$> number) <|> (List <$> parseList)

parseInput :: String -> [[Packet]]
parseInput = map (\[l, r] -> [prs l, prs r]) . splitWhen null . lines
  where
    prs s = case parse parsePacket "" s of
      Left _ -> error "bad packet"
      Right p -> p

instance Ord Packet where
  (<=) :: Packet -> Packet -> Bool
  Con x <= Con y = x <= y
  List (x : xs) <= List (y : ys) =
    case ord of
      EQ -> List xs <= List ys
      LT -> True
      GT -> False
    where
      ord = compare x y
  List [] <= List _ = True
  List (_ : _) <= List [] = False
  Con x <= List ys = List [Con x] <= List ys
  List xs <= Con y = List xs <= List [Con y]


main = do 
    packets <- parseInput <$> readFile "input"
    print $ sum [i | (i, [l, r]) <- zip [1..] packets, l <= r]

    let dividers = [List [List [Con 2]], List [List [Con 6]]]
    print $ product . map fst . filter ((`elem` dividers) . snd) 
            . zip [1..] . sort $ dividers ++ concat packets