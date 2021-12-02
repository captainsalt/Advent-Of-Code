type Command = (String, Integer)

data Position = Position
  { horizontal :: Integer,
    depth :: Integer
  }
  deriving (Show)

getDepth :: [Command] -> Position
getDepth = foldl applyCommand $ Position {horizontal = 0, depth = 0}
  where
    applyCommand :: Position -> Command -> Position
    applyCommand initPos (cmd, val)
      | cmd == "forward" = initPos {horizontal = horizontal initPos + val}
      | cmd == "backward" = initPos {horizontal = horizontal initPos - val}
      | cmd == "up" = initPos {depth = depth initPos - val}
      | cmd == "down" = initPos {depth = depth initPos + val}
      | otherwise = initPos

listToTuple :: [[Char]] -> ([Char], Integer)
listToTuple (cmd : val : _) = (cmd, read val :: Integer)
listToTuple _ = ("0", 0)

main = do
  commands <- map (listToTuple . words) . lines <$> readFile "input.txt"
  print $ (\pos -> horizontal pos * depth pos) $ getDepth commands