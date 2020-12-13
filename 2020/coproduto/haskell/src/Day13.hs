{-# LANGUAGE OverloadedStrings #-}
module Day13 where

import Debug.Trace
import Data.List (minimumBy, find)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Common (textToInteger, readFileUtf8)

part1 :: IO ()
part1 = do
  [first, second] <- Text.lines <$> readFileUtf8 "input/day_13.txt"
  let minTime = textToInteger first
  let busTimes = textToInteger <$> filter (/="x") (Text.splitOn "," second)
  let busOffsets = fmap (offset minTime) busTimes
  let (i, n) = minimumBy (\l r -> compare (snd l) (snd r)) (zip [0..] busOffsets)
  print $ (busTimes !! i) * n

part2 :: IO ()
part2 = do
  (first, rest) <- parseInput <$> readFileUtf8 "input/day_13.txt"
  print (solveMany first rest)

parseInput :: Text -> (Integer, [(Integer, Integer)])
parseInput input =
  let [_, second] = Text.lines input
      entries     = Text.splitOn "," second
      pairs       = filter ((/="x") . snd) $ zip [1..] (tail entries)
      first       = textToInteger (head entries)
      rest        = fmap (\(n, txt) -> (n, textToInteger txt)) pairs
  in (first, rest)

solveMany :: Integer -> [(Integer, Integer)] -> Integer
solveMany = solveMany' 0

solveMany' :: Integer -> Integer -> [(Integer, Integer)] -> Integer
solveMany' start _  []                = start
solveMany' start n1 ((diff, n2):rest) =
  let next = solve start n1 n2 diff
  in solveMany' next (n1 * n2) rest

solve :: Integer -> Integer -> Integer -> Integer -> Integer
solve start n1 n2 diff =
  fromJust $ find (\x -> n2 `divides` (x + diff)) ((start +) <$> (multiples n1))

multiples :: (Num a, Enum a) => a -> [a]
multiples n = (n *) <$> [1..]

divides :: Integral a => a -> a -> Bool
divides b a = a `mod` b == 0

offset :: Integral a => a -> a -> a
offset a b = b * (a `div` b + 1) - a
