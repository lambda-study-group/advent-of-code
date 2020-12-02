module Main where

import Data.IntSet (IntSet, member)
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)),Seq((:|>)))
import Criterion.Main

-- Simple and naive implementation O(n^2)
naive2 :: [Int] -> (Int, Int)
naive2 xs = head [(x,y) | x <- xs, y <- xs, x+y==2020]

-- Simple and naive implementation O(n^3)
naive3 :: [Int] -> (Int, Int, Int)
naive3 xs = head [(x,y,z) | x <- xs, y <- xs, z <- xs, x+y+z==2020]

-- Using IntSet to store the data and perform O(1) queries 
-- O(n) complexity (since it uses Patricia Tree and the index is Int)
withSet :: Int -> [Int] -> IntSet -> Maybe (Int, Int)
withSet tot [] ref  = Nothing
withSet tot (x:xs) ref
  | y `member` ref = Just (x,y)
  | otherwise      = withSet tot xs ref
  where
    y = tot - x

-- Using `withSet` we can make part 2 in O(n^2)
withSet3 :: Int -> [Int] -> IntSet -> Maybe (Int, Int, Int)
withSet3 tot [] ref = Nothing
withSet3 tot (x:xs) ref = let remaining = tot - x
                             in  case withSet remaining xs ref of
                                    Nothing -> withSet3 tot xs ref
                                    Just (y,z) -> Just (x,y,z)
        
-- Generic function for any quantity of numbers
-- and any value of total (but really slow)
sumTotWithN :: Int -> Int -> [Int] -> [[Int]]
sumTotWithN tot n xs = go n xs [[]]
  where
    go 0 _ ys = filter ((==tot).sum) ys
    go n [] _ = []
    go n (x:xs) ys = let zs = filter ((<=tot).sum) $ map (x:) ys
                      in go (n-1) xs zs ++ go n xs ys

-- Using Finger Trees to:
-- sort the list in O(n log n)
-- Check if the extremes add to the total value
-- if not, either we move the left pointer forward, increasing the value
-- or move the right pointer backwards, decreasing the value
withSeq :: Int -> Seq.Seq Int -> (Int, Int)
withSeq tot xs = findMatch $ Seq.sort xs
  where
    findMatch (x :<| (xs :|> y))
      | x+y == tot = (x,y)
      | x+y <  tot = findMatch (xs :|> y)
      | otherwise  = findMatch (x :<| xs)

main :: IO ()
main = do
  dat <- map read . lines <$> readFile "day01.txt"
  let sdat   = IS.fromList dat
      seqdat = Seq.fromList dat
      seqTest = withSeq 2020
      genericTest xs = head $ sumTotWithN 2020 2 xs
      setTest = withSet 2020 dat
  -- some benchmarking
  defaultMain [
    bgroup "two" [ bench "naive" $ whnf naive2 dat
                   , bench "generic" $ whnf genericTest dat
                   , bench "withSet" $ whnf setTest sdat
                   , bench "binsearch" $ whnf seqTest seqdat
                   ]
    ,bgroup "three" [ bench "naive" $ whnf naive3 dat
                    , bench "generic" $ whnf (sumTotWithN 2020 3) dat
                    , bench "withSet" $ whnf (withSet3 2020 dat) sdat
                    ]
     ]
  let (x, y) = naive2 dat
      (a,b,c) = naive3 dat      
      Just (x', y') = withSet 2020 dat sdat
      Just (a', b', c') = withSet3 2020 dat sdat
      (xs, ys) = withSeq 2020 $ Seq.fromList dat
  print (x*y)
  print (x'*y')
  print (xs*ys)
  print $ product $ head $ sumTotWithN 2020 2 dat
  print (a*b*c)  
  print $ product $ head $ sumTotWithN 2020 3 dat
  print (a'*b'*c')
