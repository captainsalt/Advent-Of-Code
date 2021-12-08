import Data.Bifunctor (Bifunctor (bimap))
import Data.List (unfoldr)

parseInput :: [Char] -> [Integer]
parseInput = unfoldr parse
  where
    parse "" = Nothing
    parse s = Just . bimap read (drop 1) . break (== ',') $ s

getCost :: Int -> Int
getCost = (map run [0 ..] !!)
  where
    run 0 = 0
    run n = getCost (n - 1) + n

average :: [Integer] -> Integer
average xs = floor $ realToFrac (sum xs) / realToFrac (length xs)

combinations :: [Integer] -> [Integer]
combinations input = map (toInteger . cost) input
  where
    avg = average input
    cost = getCost . fromInteger . abs . (-) avg

main :: IO ()
main = do
  fileContent <- readFile "input.txt"
  print $ sum . combinations . parseInput $ fileContent