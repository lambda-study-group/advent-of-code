module Main where

import qualified Day1
import qualified Day2

main :: IO ()
main = do
  putStrLn "-- Day 1 --"
  putStr "Day 1 Part 1 - Result: "
  Day1.part1
  putStr "Day 1 Part 2 - Result: "
  Day1.part2
  putStr "\n"

  putStrLn "-- Day 2 --"
  putStr "Day 2 Part 1 - Result: "
  Day2.part1
  putStr "Day 2 Part 2 - Result: "
  Day2.part2
  putStr "\n"
