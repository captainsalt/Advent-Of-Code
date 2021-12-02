type Command = (String, Integer)

data Position = Position
  { horizontal :: Integer,
    depth :: Integer,
    aim :: Integer
  }
  deriving (Show)

getDepth :: [Command] -> Position
getDepth = foldl applyCommand $ Position {horizontal = 0, depth = 0, aim = 0}
  where
    applyCommand :: Position -> Command -> Position
    applyCommand start (cmd, val)
      | cmd == "forward" = start {horizontal = horizontal start + val, depth = depth start + aim start * val}
      | cmd == "up" = start {aim = aim start - val}
      | cmd == "down" = start {aim = aim start + val}
      | otherwise = start

listToTuple :: [[Char]] -> ([Char], Integer)
listToTuple (cmd : val : _) = (cmd, read val :: Integer)
listToTuple _ = ("0", 0)

main = do
  commands <- map (listToTuple . words) . lines <$> readFile "input.txt"
  print $ (\pos -> horizontal pos * depth pos) $ getDepth commands