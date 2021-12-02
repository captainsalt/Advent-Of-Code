countIncrements :: [Integer] -> Integer
countIncrements input = countIncrements' input 0
  where
    countIncrements' :: [Integer] -> Integer -> Integer
    countIncrements' [] _ = 0
    countIncrements' [_] count = count
    countIncrements' (x : xs) count
      | head xs > x = countIncrements' xs (succ count)
      | otherwise = countIncrements' xs count

main :: IO ()
main = do
  fileContent <- lines <$> readFile "input.txt"
  print $ countIncrements $ map read fileContent