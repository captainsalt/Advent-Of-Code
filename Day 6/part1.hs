import Data.Bifunctor (Bifunctor (bimap))
import Data.Bool (bool)
import Data.List (unfoldr)

type Days = Int

type Timers = Int

type Population = Int

parseTimers :: String -> [Timers]
parseTimers = unfoldr parse
  where
    parse "" = Nothing
    parse s = Just . bimap read (drop 1) $ break (== ',') s

simulate :: Days -> [Timers] -> [Timers]
simulate 0 timers = timers
simulate days timers = simulate (days - 1) updatedTimers
  where
    nextDay = pred <$> timers
    newTimers = flip replicate 8 . length . filter (== -1) $ nextDay
    updatedTimers = [bool x 6 $ x == -1 | x <- nextDay] <> newTimers

main :: IO ()
main = do
  fileContent <- readFile "input.txt"

  print $
    length
      . simulate 80
      . parseTimers
      $ fileContent