import Data.Bifunctor (Bifunctor (second))
import Data.Bool (bool)
import Data.Char (digitToInt)
import Data.Maybe ()
import qualified Data.Maybe

data Rating = OxygenGenerator | C02Scrubber

type Bit = Char

type Index = Int

type Binary = [Bit]

binaryToInt :: Binary -> Int
binaryToInt = run 0
  where
    run :: Int -> Binary -> Int
    run num [] = num
    run num binary =
      let bitValue = digitToInt $ head binary
          power = length binary - 1
          newNum = (bitValue * 2 ^ power) + num
          newBinary = tail binary
       in run newNum newBinary

getBitCriteria :: Rating -> [Binary] -> Bit
getBitCriteria rating binList =
  let column = map head binList
      oneCount = length $ filter (== '1') column
      zeroCount = length $ filter (== '0') column
   in case rating of
        OxygenGenerator -> bool '0' '1' $ oneCount >= zeroCount
        C02Scrubber -> bool '0' '1' $ oneCount < zeroCount

diagnosticData :: Rating -> [Binary] -> Binary
diagnosticData rating binList = run rating $ map (\b -> (b, b)) binList
  where
    run :: Rating -> [(Binary, Binary)] -> Binary
    run _ [x] = fst x
    run rating binList =
      let magicBit = getBitCriteria rating $ map snd binList
          newList =
            [ second tail (bin, acc)
              | (bin, acc) <- binList,
                head acc == magicBit
            ]
       in run rating newList

main = do
  fileContent <- lines <$> readFile "input.txt"

  let oxygen = diagnosticData OxygenGenerator fileContent
  let c02 = diagnosticData C02Scrubber fileContent

  print $ (\x -> (x, binaryToInt x)) oxygen
  print $ (\x -> (x, binaryToInt x)) c02
  print $ product $ map binaryToInt [oxygen, c02]
