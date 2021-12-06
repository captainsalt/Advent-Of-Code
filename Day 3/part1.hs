import Data.Bool (bool)
import Data.Char

data Rate = Gamma | Epsilon

type Bit = Char

type Position = Int

type Binary = [Bit]

groupColumns :: [Binary] -> [Binary]
groupColumns = run []
  where
    run :: [[Binary]] -> [Binary] -> [Binary]
    run acc binaryList
      | head binaryList == "" = reverse . concat $ acc
      | otherwise =
        let column = map head binaryList
            newAcc = [column] : acc
            newBinaryList = map tail binaryList
         in run newAcc newBinaryList

getSignificantBits :: Rate -> [Binary] -> Binary
getSignificantBits rate = map (getBit rate)
  where
    getBit :: Rate -> Binary -> Bit
    getBit rate binary =
      let oneCount = length $ filter (== '1') binary
          zeroCount = length $ filter (== '0') binary
       in case rate of
            Gamma -> bool '0' '1' $ oneCount > zeroCount
            Epsilon -> bool '0' '1' $ oneCount < zeroCount

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

main :: IO ()
main = do
  fileContent <- lines <$> readFile "input.txt"

  let gamma = binaryToInt $ getSignificantBits Gamma $ groupColumns fileContent
  let epsilon = binaryToInt $ getSignificantBits Epsilon $ groupColumns fileContent

  print $ gamma * epsilon