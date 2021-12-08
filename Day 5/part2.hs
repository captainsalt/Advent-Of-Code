import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (fold))
import Data.List (group, sort, transpose, unfoldr)

type Coordinate = (Int, Int)

data Line = Line
  { start :: Coordinate,
    end :: Coordinate
  }
  deriving (Show)

parseLine :: String -> Line
parseLine =
  listToLine
    . map (fmap (drop 1) . break (== ','))
    . filter (not . (==) "->")
    . words
  where
    listToLine [a, b] = Line (bimap read read a) (bimap read read b)
    listToLine _ = Line (-1, -1) (-1, -1)

parseLines :: [String] -> [Line]
parseLines = map parseLine

drawLine :: Line -> [Coordinate]
drawLine line = draw (start line)
  where
    Line {start = c1@(x1, y1), end = c2@(x2, y2)} = line

    draw :: Coordinate -> [Coordinate]
    draw current
      | current == c2 = [c1]
      | otherwise = updatedCurrent : draw updatedCurrent
      where
        xDirection = bool (-1) 1 $ x1 < x2
        yDirection = bool (-1) 1 $ y1 < y2
        addXDirection pos = bool pos (first (xDirection +) pos) $ x1 /= x2
        addYDirection pos = bool pos (second (yDirection +) pos) $ y1 /= y2
        addDirections = addYDirection . addXDirection
        updatedCurrent = addDirections current

        addXDirection :: Coordinate -> Coordinate
        addYDirection :: Coordinate -> Coordinate
        addDirections :: Coordinate -> Coordinate

drawLines :: [Line] -> [Coordinate]
drawLines = concatMap drawLine

main :: IO ()
main = do
  fileContent <- lines <$> readFile "input.txt"

  print $
    length . filter ((<) 1 . length)
      . group
      . sort
      . drawLines
      . parseLines
      $ fileContent

  return ()