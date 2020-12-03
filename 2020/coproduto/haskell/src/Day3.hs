module Day3
  ( part1
  , part2
  ) where

import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text

import Common (readFileUtf8)

part1 :: IO ()
part1 = printSlopeCheck [(3, 1)]

part2 :: IO ()
part2 = printSlopeCheck [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

printSlopeCheck :: [(Int, Int)] -> IO ()
printSlopeCheck slopes = do
  inputLines <- Text.lines <$> readFileUtf8 "input/day_3.txt"
  print (checkSlopes slopes inputLines)

checkSlopes :: [(Int, Int)] -> [Text] -> Int
checkSlopes slopes lines = foldl' (*) 1 (fmap (checkSlope lines) slopes)

checkSlope :: [Text] -> (Int, Int) -> Int
checkSlope lines slope = length . filter (== '#') $ encounteredObjects
  where encounteredObjects   = getCharsAtPositions lines encounteredPositions
        encounteredPositions = positions (Text.length $ head lines) slope

positions :: Int -> (Int, Int) -> [(Int, Int)]
positions max (dx, dy) = iterate (advance max dx dy) (0, 0)
  where advance max dx dy (x, y) =
          ((x + dx) `mod` max, y + dy)

getCharsAtPositions :: [Text] -> [(Int, Int)] -> [Char]
getCharsAtPositions lines positions = catMaybes (go lines positions 0)
  where go []     _            n = []
        go _      []           n = []
        go (l:ls) p@((x,y):ps) n =
          let elem = if y == n then Just (Text.index l x) else Nothing
              p'   = if y == n then ps else p
              rest = go ls p' (n + 1)
          in elem : rest
