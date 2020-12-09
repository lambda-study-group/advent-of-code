module Main where

import Data.List
import Data.IntSet (IntSet(), member, fromList)
import qualified Data.IntSet as S 
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq

getPreambles :: Int -> [Int] -> [IntSet]
getPreambles n = map (fromList . take n) . tails

checkSum :: IntSet -> Int -> Bool
checkSum key val = S.foldr checkRemainder False key
  where
    checkRemainder k b = (val-k) `member` key || b 

findSum :: Int -> [Int] -> Seq Int
findSum 0 _  = Seq.Empty 
findSum n xs = go xs Seq.empty 0
  where
    go (x:xs) Empty tot = go xs (Seq.singleton x) (tot+x)
    go (x:xs) s@(y :<| t) tot
      | tot == n = s
      | tot >  n = go (x:xs) t (tot-y)
      | tot <  n = go xs (s |> x) (tot+x)

main :: IO ()
main = do
  numbers <- map read . lines <$> readFile "day09.txt"
  let n     = 25
      keys  = getPreambles n numbers
      vals  = drop n numbers
      kv    = zip keys vals
      part1 = snd . head $ filter (not . uncurry checkSum) kv
      part2 = findSum part1 numbers 
  print part1
  print $ minimum part2 + maximum part2
