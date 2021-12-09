import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.List (group, sort, sortBy, sortOn, unfoldr)

type Days = Int

type Timer = Int

type SpawnCount = Int

data Fish = Fish
  { population :: Int,
    spawnTimer :: Timer
  }
  deriving (Show, Eq)

parseFish :: String -> [Fish]
parseFish = timerToFish . unfoldr parse
  where
    parse "" = Nothing
    parse s = Just . bimap read (drop 1) $ break (== ',') s

    timerToFish = map listToFish . group . sort
      where
        listToFish list = Fish {population = length list, spawnTimer = head list}

spawnFish :: Int -> [Fish] -> [Fish]
spawnFish quantity fish = case quantity of
  0 -> fish
  _ -> Fish {population = quantity, spawnTimer = 8} : fish

simulateDay :: [Fish] -> [Fish]
simulateDay = run 0
  where
    run :: SpawnCount -> [Fish] -> [Fish]
    run spawnCount [] = spawnFish spawnCount []
    run spawnCount (fish@Fish {spawnTimer = timer, population = pop} : xs) =
      fish {spawnTimer = fst timerUpdate} :
      run (spawnCount + snd timerUpdate) xs
      where
        timerUpdate :: (Timer, SpawnCount)
        timerUpdate = bool (timer - 1, 0) (6, pop) $ timer - 1 == -1

simulate :: Days -> [Fish] -> Int
simulate 0 fish = sum . map population $ fish
simulate days fish = simulate (days - 1) (simulateDay fish)

main :: IO ()
main = do
  fileContent <- readFile "input.txt"

  print $
    simulate 256 $
      parseFish fileContent