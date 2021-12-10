import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.List (group, groupBy, sort, sortBy, sortOn, unfoldr)

type Days = Integer

type Timer = Integer

type SpawnCount = Integer

data Fish = Fish
  { population :: Integer,
    spawnTimer :: Timer
  }
  deriving (Eq)

instance Show Fish where
  show Fish {population = pop, spawnTimer = timer} =
    "Timer: "
      <> show timer
      <> " Population: "
      <> show pop
      <> "\n"

addFish :: Fish -> Fish -> Fish
addFish Fish {population = aPop, spawnTimer = aTimer} Fish {population = bPop, spawnTimer = bTimer} =
  Fish {population = aPop + bPop, spawnTimer = aTimer}

-- combineFish :: [Fish] -> [Fish]
combineFish = map (foldl1 addFish) . groupBy (\(f1, f2) -> (spawnTimer f1) == (spawnTimer f2)) . sortOn spawnTimer

parseFish :: String -> [Fish]
parseFish = timerToFish . unfoldr parse
  where
    parse "" = Nothing
    parse s = Just . bimap read (drop 1) $ break (== ',') s

    timerToFish = map listToFish . group . sort
      where
        listToFish list = Fish {population = toInteger $ length list, spawnTimer = head list}

spawnFish :: Integer -> [Fish] -> [Fish]
spawnFish quantity fish = case quantity of
  0 -> fish
  _ -> Fish {population = quantity, spawnTimer = 8} : fish

simulateDay :: [Fish] -> [Fish]
simulateDay = combineFish . run 0
  where
    run :: SpawnCount -> [Fish] -> [Fish]
    run spawnCount [] = spawnFish spawnCount []
    run spawnCount (fish@Fish {spawnTimer = timer, population = pop} : xs) =
      fish {spawnTimer = fst timerUpdate} :
      run (spawnCount + snd timerUpdate) xs
      where
        timerUpdate :: (Timer, SpawnCount)
        timerUpdate = bool (timer - 1, 0) (6, pop) $ timer - 1 == -1

-- simulate :: Days -> [Fish] -> Integer
simulate 0 fish = combineFish fish
simulate days fish = combineFish $ simulate (days - 1) (combineFish $ simulateDay fish)

main :: IO ()
main = do
  fileContent <- readFile "input.txt"

  print $
    simulate 10 $
      parseFish fileContent