{-# LANGUAGE OverloadedStrings #-}

module Day6
  ( part1
  , part2
  ) where

import Data.List (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set

import Common (readFileUtf8)

part1 :: IO ()
part1 = countSetEntries (foldl' Set.union Set.empty)

part2 :: IO ()
part2 = countSetEntries (foldl' Set.intersection (Set.fromAscList ['a'..'z']))

countSetEntries :: ([Set Char] -> Set Char) -> IO ()
countSetEntries collect = do
  answerSets <- getAnswerSets collect <$> readFileUtf8 "input/day_6.txt"
  print (sum $ Set.size <$> answerSets)

getAnswerSets :: ([Set Char] -> Set Char) -> Text -> [Set Char]
getAnswerSets collect input = input
  &   Text.splitOn "\n\n"
  <&> Text.lines
  <&> (fmap $ Set.fromList . Text.unpack)
  <&> collect
