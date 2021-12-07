import Data.Bifunctor (Bifunctor (first))
import Data.Char (digitToInt)
import Data.List (find, transpose, uncons)
import Data.Maybe (isJust, isNothing)

data BoardValue = Picked Int | NotPicked Int deriving (Show, Eq)

type Board = [[BoardValue]]

type BoardList = [Board]

type DrawnNumbers = [Int]

type Draw = Int

parseDrawnNumbers :: String -> [Int]
parseDrawnNumbers s = case dropWhile (',' ==) s of
  "" -> []
  s' -> read w : parseDrawnNumbers s''
    where
      (w, s'') = break (',' ==) s'

parseBoards :: [String] -> BoardList
parseBoards = parse . map (map (NotPicked . read) . words)
  where
    parse :: Board -> BoardList
    parse [] = []
    parse values = five : parse rest
      where
        (five, rest) = splitAt 5 values

isPicked :: BoardValue -> Bool
isPicked (Picked _) = True
isPicked _ = False

isWinningBoard :: Board -> Maybe Board
isWinningBoard board = case find (all isPicked) $ board ++ transpose board of
  Nothing -> Nothing
  _ -> Just board

boardValueToInt :: BoardValue -> Int
boardValueToInt value = case value of
  Picked n -> n
  NotPicked n -> n

pickNumber :: Int -> Board -> Board
pickNumber pick = map (run pick)
  where
    run :: Int -> [BoardValue] -> [BoardValue]
    run pick row =
      [ case boardValue of
          bValue@(NotPicked v) -> if v == pick then Picked v else bValue
          bValue -> bValue
        | boardValue <- row
      ]

callNumber :: Int -> BoardList -> BoardList
callNumber pick = map (pickNumber pick)

getLosingBoard :: DrawnNumbers -> BoardList -> Maybe (Board, Draw)
getLosingBoard [] _ = Nothing
getLosingBoard drawNums boards =
  let drawn = head drawNums
      updatedBoards = callNumber drawn boards
      losingBoards = filter (isNothing . isWinningBoard) updatedBoards
   in case losingBoards of
        [] -> Just (head updatedBoards, drawn)
        _ -> getLosingBoard (tail drawNums) losingBoards

main :: IO ()
main = do
  let filePath = "input.txt"
  Just (drawnNums, boardRows) <- uncons . filter (not . null) . lines <$> readFile filePath

  let numbers = parseDrawnNumbers drawnNums
  let boards = parseBoards boardRows
  let result = getLosingBoard numbers boards

  case result of
    Nothing -> print "No winner"
    Just (board, pick) -> do
      print board
      let sumFilter = sum . map boardValueToInt . flip concatMap board
          notPicked = sumFilter (filter $ not . isPicked)
      print $ notPicked * pick