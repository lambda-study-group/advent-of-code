module Main where

import Data.IntMap.Strict

genList [x]    n s = x : next    0  (n+1) (insert x n s)
genList (x:xs) n s = x : genList xs (n+1) (insert x n s)

next x n s = 
  case s !? x of
    Nothing -> x : next 0     (n+1) (insert x n s)
    Just y  -> x : next (n-y) (n+1) (insert x n s)
               
main :: IO ()
main = do
  let list = genList [15,12,0,14,3,1] 1 empty
  print $ take 100 list
  print $ list !! (2020 - 1)
  print $ list !! (30000000 - 1)
