{-# LANGUAGE TupleSections #-}

module Day9 where

import Data.List (foldl')
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Sequence (Seq(Empty, (:<|)), (><), (|>))
import qualified Data.Sequence as Seq
import Control.Monad.State

import Common (textToInt, readFileUtf8)

part1 :: IO ()
part1 = do
  nums <- parseInput <$> readFileUtf8 "input/day_9.txt"
  print (runPart1 nums)

part2 :: IO ()
part2 = do
  nums <- parseInput <$> readFileUtf8 "input/day_9.txt"
  let target   = runPart1 nums
  let window   = findWindow target nums Seq.empty
  let smallest = minimum window
  let largest  = maximum window
  print (smallest + largest)

runPart1 :: [Int] -> Int
runPart1 nums = findInvalid 25 (drop 25 nums) (Seq.fromList $ take 25 nums)

findWindow :: Int -> [Int] -> Seq Int -> Seq Int
findWindow target rest@(n:ns) window
  | sum' window == target = window
  | sum' window <  target = findWindow target ns (window |> n)
  | sum' window >  target = findWindow target rest (Seq.drop 1 window)

findInvalid :: Int -> [Int] -> Seq Int -> Int
findInvalid len (n:ns) window
  | findSum n (initSums window) = findInvalid len ns (Seq.drop 1 window |> n)
  | otherwise                   = n

parseInput :: Text -> [Int]
parseInput = fmap textToInt . Text.lines

initSums :: Seq Int -> Seq Int
initSums nums = do
  x <- nums
  y <- nums
  if x /= y then Seq.singleton (x + y) else Seq.empty

findSum :: Int -> Seq Int -> Bool
findSum _ Empty         = False
findSum x (x' :<| rest) = x == x' || findSum x rest

sum' :: (Foldable f, Num a) => f a -> a
sum' = foldl' (+) 0
