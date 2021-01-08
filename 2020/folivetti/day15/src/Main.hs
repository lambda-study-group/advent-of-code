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

reduceM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
reduceM x xs f  = foldM f x xs
reduceM_ x xs f = foldM_ f x xs

nextNumber _    (-1) = 0
nextNumber turn val  = turn - val

run :: Int -> [Int] -> Int
run n input = runST $ do
  arr <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
  reduceM_ 1 input (\turn x -> do writeArray arr x turn; return (turn+1))
  reduceM 0 [length input + 1 .. n - 1] 
    (\next turn -> do val <- nextNumber turn <$> readArray arr next
                      writeArray arr next turn
                      return val) 

main :: IO ()
main = do
  --let list = genList [15,12,0,14,3,1] 1 empty
  --print $ list !! (2020 - 1)
  --print $ list !! (30000000 - 1)
  print $ run 2020 [15,12,0,14,3,1]
  print $ run 30000000 [15,12,0,14,3,1]
