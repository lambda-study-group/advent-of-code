module Day5
  ( part1
  , part2
  ) where

import Data.List (foldl', sort, find)
import Data.Text (Text)
import qualified Data.Text as Text

import Common (readFileUtf8)

part1 :: IO ()
part1 = do
  seatIds <- getSeatIds <$> readFileUtf8 "input/day_5.txt"
  print (maximum seatIds)

part2 :: IO ()
part2 = do
  seatIds <- getSeatIds <$> readFileUtf8 "input/day_5.txt"
  case findGap seatIds of
    Just n -> print n
    Nothing -> putStrLn "Invalid input."


getSeatIds :: Text -> [Int]
getSeatIds input =
  getSeatId <$> Text.unpack <$> Text.lines input

getSeatId :: [Char] -> Int
getSeatId chars = seatRow * 8 + seatColumn
  where seatRow    = getSeatRow (take 7 chars)
        seatColumn = getSeatColumn (drop 7 chars)

getSeatRow :: [Char] -> Int
getSeatRow = foldl' addDigit 0
  where addDigit n 'F' = 2 * n
        addDigit n 'B' = 2 * n + 1

getSeatColumn :: [Char] -> Int
getSeatColumn = foldl' addDigit 0
  where addDigit n 'L' = 2 * n
        addDigit n 'R' = 2 * n + 1
  

findGap :: [Int] -> Maybe Int
findGap ns =
  let sorted = sort ns
      diffs = zipWith (-) sorted (tail sorted)
      withDiffs = zip sorted diffs
  in (+1) <$> fst <$> find ((==(-2)) . snd) withDiffs
