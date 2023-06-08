{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Text.Parsec.String

integer :: Parser Integer
integer = read <$> many1 digit

section :: Parser (Integer, Integer)
section = do
    min <- integer
    char '-'
    max <- integer
    return (min, max)

pair :: Parser ((Integer, Integer), (Integer, Integer))
pair = do
    s1 <- section
    char ','
    s2 <- section
    return (s1, s2)

pairs = do
  s <- getLine
  if null s
    then return []
    else do
      ps <- pairs
      let p = parse pair "" s
      return (p : ps)

getpairs = do
    ps <- sequenceA <$> pairs
    case ps of
        Left _ -> error "not valid"
        Right xs -> return xs
    
subsumes ((a, b), (c, d)) = (c <= a && b <= d)
                         || (a <= c && d <= b)

countFullyContained :: IO Int
countFullyContained = do
    ps <- getpairs
    return $ sum [1 | p <- ps, subsumes p]

partial ((a, b), (c, d)) = (a <= c && c <= b)
                         || (a <= d && d <= b)

countOverlapping :: IO Int
countOverlapping = do
    ps <- getpairs
    return $ sum [1 | p <- ps, partial p || subsumes p]