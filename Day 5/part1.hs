import Data.Bifunctor (Bifunctor (bimap))
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

-- TODO: Refactor
drawLine :: Line -> [Coordinate]
drawLine Line {start = (x1, y1), end = (x2, y2)}
  | x1 /= x2 && y1 == y2 =
    [ (val, y1)
      | let (start, end) = startEnd x1 x2,
        val <- [start .. end]
    ]
  | y1 /= y2 && x1 == x2 =
    [ (x1, val)
      | let (start, end) = startEnd y1 y2,
        val <- [start .. end]
    ]
  | otherwise = []
  where
    startEnd :: Int -> Int -> (Int, Int)
    startEnd a b = (min a b, max a b)

drawLines :: [Line] -> [Coordinate]
drawLines = concatMap drawLine

main :: IO ()
main = do
  fileContent <- lines <$> readFile "input.txt"

  print $
    length
      . filter ((<) 1 . length)
      . group
      . sort
      . drawLines
      . parseLines
      $ fileContent

  return ()