{-# LANGUAGE LambdaCase #-}

module Day10 where

import Data.List (sort, tails)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Common (sum', textToInt, readFileUtf8)

part1 :: IO ()
part1 = do
  input <- parseInput <$> readFileUtf8 "input/day_10.txt"
  let (one, three) = countDiffs (0 : (sort input))
  print (one * three)

part2 :: IO ()
part2 = do
  input <- parseInput <$> readFileUtf8 "input/day_10.txt"
  print (arrangements (0 : input))

arrangements :: [Int] -> Int
arrangements nums =
  let nums' = Vector.fromList (sort nums)
      v     = Vector.generate (length nums) countArrangements
      countArrangements x
        | x == (length nums) - 1 = 1
        | otherwise =
            let valids = Vector.takeWhile ((<= 3) . (subtract (nums' ! x))) (Vector.drop (x + 1) nums')
                indices = [(x + 1)..(x + (length valids))]
            in sum' ((v !) <$> indices)
  in v ! 0

countDiffs :: [Int] -> (Int, Int)
countDiffs = countDiffs' (0, 0)
  where
    countDiffs' (one, three) (x:y:xs)
      | y - x == 1 = countDiffs' (one + 1, three) (y:xs)
      | y - x == 3 = countDiffs' (one, three + 1) (y:xs)
      | otherwise  = countDiffs' (one, three) (y:xs)
    countDiffs' (one, three) (x:xs) = (one, three + 1)

parseInput :: Text -> [Int]
parseInput = fmap textToInt . Text.lines
