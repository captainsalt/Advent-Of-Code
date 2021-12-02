countIncrements :: [Integer] -> Integer
countIncrements input = run input 0
  where
    run :: [Integer] -> Integer -> Integer
    run [] _ = 0
    run [_] count = count
    run (x : xs) count
      | head xs > x = run xs (succ count)
      | otherwise = run xs count

toSlidingWindow :: [Integer] -> [[Integer]]
toSlidingWindow input = run input []
  where
    run :: [Integer] -> [[Integer]] -> [[Integer]]
    run [] windows = reverse windows
    run input windows = run (tail input) $ take 3 input : windows

main :: IO ()
main = do
  fileContent <- map read . lines <$> readFile "input.txt"
  print $ countIncrements . map sum $ toSlidingWindow fileContent