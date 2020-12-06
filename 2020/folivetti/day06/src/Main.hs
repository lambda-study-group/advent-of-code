module Main where

import Data.List.Split  (splitOn)
import Data.Set         (fromList, intersection, unions, size) 

main :: IO ()
main = do
  content <- map (map fromList . lines) . splitOn "\n\n" <$> readFile "day06.txt"
  let 
    part1  = map (size . unions) content
    part2  = map (size . foldr1 intersection) content
  print $ sum part1
  print $ sum part2
