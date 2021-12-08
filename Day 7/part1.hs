import Data.Bifunctor (Bifunctor (bimap))
import Data.List (unfoldr)

parseInput :: [Char] -> [Int]
parseInput = unfoldr parse
  where
    parse "" = Nothing
    parse s = Just . bimap read (drop 1) . break (== ',') $ s

combinations :: [Int] -> [Int]
combinations [] = []
combinations input = run (length input) input
  where
    run 0 input = []
    run count input = sum [abs (input !! (count - 1) - val) | val <- input] : run (count - 1) input

main :: IO ()
main = do
  fileContent <- readFile "input.txt"
  print $ minimum . combinations . parseInput $ fileContent
