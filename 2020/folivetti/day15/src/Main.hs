{-# language FlexibleContexts #-}
module Main where

import Data.IntMap.Strict
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

genList [x]    n s = x : next    0  (n+1) (insert x n s)
genList (x:xs) n s = x : genList xs (n+1) (insert x n s)

next x n s = 
  case s !? x of
    Nothing -> x : next 0     (n+1) (insert x n s)
    Just y  -> x : next (n-y) (n+1) (insert x n s)

run :: Int -> [Int] -> Int
run n input = runST $ do
  arr <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
  let init (i, v) = writeArray arr v i
      f next turn = do val <- readArray arr next
                       writeArray arr next turn
                       if val == -1 
                          then return 0
                          else return (turn-val)
  mapM_ init $ zip [1..] input 
  foldM f 0 [length input + 1 .. n - 1]

main :: IO ()
main = do
  --let list = genList [15,12,0,14,3,1] 1 empty
  --print $ list !! (2020 - 1)
  --print $ list !! (30000000 - 1)
  print $ run 2020 [15,12,0,14,3,1]
  print $ run 30000000 [15,12,0,14,3,1]
