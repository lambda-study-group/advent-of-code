module Main where

import Data.List
import Control.Monad
import Algebra
import Representable

genDynTable :: CoAlgebra (ListF Int) [Int]
genDynTable []     = NilF
genDynTable (x:xs) = ConsF x xs

count :: Int -> [Int] -> Algebra (ListF Int) [Int]
-- The final adapter is always the end point, this counts as 1
count _ _     NilF         = [1]
-- The x-th adapter is the sum of the total ways we can connect
-- adapters x+1, x+2, x+3, if they are within the range
count n jolts (ConsF x xs) = tot : xs
  where
    -- we can't get beyond index n 
    rng  = filter (<n) [x+1..x+3]
    jolt = jolts !! x
    tot = foldr sum' 0 rng 
    sum' ix acc
      | (jolts !! ix) - jolt <= 3 = acc + (xs !! (ix-x-1))
      | otherwise                 = 0

f :: Int -> [Int] -> Int -> Int
f n jolts ix
  | ix == n-2 = 1
  | otherwise = sum $ map (f n jolts) rng
  where
    rng  = filter (\jx -> (jx < n) && within jx) [ix+1..ix+3]
    jolt = jolts !! ix
    within jx = jolts !! jx <= jolt + 3

main :: IO ()
main = do
  dat <- map read . lines <$> readFile "day10.txt"
  let jolts = 0 : sort dat
      diff  = zipWith (-) (tail jolts) jolts 
      n     = length jolts
      n1    = length $ filter (==1) diff
      n3    = length $ filter (==3) diff
      t :: Stream Int
      t     = tabulate (f n jolts)
  print $ n1*(n3+1)
  print $ head $ hylo (count n jolts) genDynTable [0 .. length jolts - 2]
  print $ index t 0
