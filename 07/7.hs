import Data.List.Split

data Command = Cd String | Ls [FS] deriving (Show)

data FS = Dir String [FS] | File String Int deriving (Show)

parseCommands :: [[String]] -> [Command]
parseCommands =
  map
    ( \c ->
        case words (head c) of
          ["cd", dir] -> Cd dir
          ["ls"] -> Ls $ parseList (tail c)
    )
  where
    parseList =
      map
        ( \l ->
            case words l of
              ["dir", name] -> Dir name []
              [size, name] -> File name (read size)
        )

populateFiles :: [FS] -> [String] -> FS -> FS
populateFiles _ _ (File n s) = File n s
populateFiles files (d : pwd) (Dir n s)
  | n == d = case pwd of
      [] -> Dir n files
      _ -> Dir n (map (populateFiles files pwd) s)
  | otherwise = Dir n s

processCommand :: ([String], FS) -> Command -> ([String], FS)
processCommand (pwd, fs) (Cd "..") = (init pwd, fs)
processCommand (pwd, fs) (Cd dir) = (pwd ++ [dir], fs)
processCommand (pwd, fs) (Ls files) = (pwd, populateFiles files pwd fs)

sumDirSizes :: Int -> FS -> (Int, Int)
sumDirSizes l (File _ s) = (s, 0)
sumDirSizes l (Dir _ sub)
  | size <= l = (size, val+size)
  | otherwise = (size, val)
  where
    (sizes, dirs) = unzip $ map (sumDirSizes l) sub
    size = sum sizes
    val = sum dirs

usedSpace :: FS -> Int
usedSpace (File _ s) = s
usedSpace (Dir _ sub) = sum $ map usedSpace sub

atLeast :: Int -> FS -> (Int, [Int])
atLeast l (File _ s) = (s, [])
atLeast l (Dir _ sub)
  | l <= size = (size, size : vals )
  | otherwise = (size, vals)
  where
    (sizes, dirs) = unzip $ map (atLeast l) sub
    size = sum sizes
    vals = concat dirs

main = do
  input <- readFile "input"
  let commands = parseCommands $ filter (not . null) . map lines . splitOn "$ " $ input
  let filesystem = snd $ foldl processCommand ([], Dir "/" []) commands
  print $ snd $ sumDirSizes 100000 filesystem
  print $ minimum $ snd $ atLeast (usedSpace filesystem - 40000000) filesystem
  