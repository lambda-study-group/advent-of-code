module Day6 where

import Data.List (nub, intersect)


splitBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitBy pred xs =
  let
    ys = takeWhile (not . pred) xs
    zs = dropWhile (not . pred) xs
  in
    if (length zs == 0)
       then ys : []
       else ys : splitBy pred (tail zs)

{-@ sum' :: [Nat] -> Nat @-}
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs


{-@ solve1 :: String -> Nat @-}
solve1 :: String -> Int
solve1 = sum' . fmap (length . nub . concat) . splitBy (== "") . lines

{-@ solve2 :: String -> Nat @-}
solve2 :: String -> Int
solve2 = sum' . fmap (length . foldl intersect u) . splitBy (== "") . lines
  where u = ['a' .. 'z']


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath
  print $ solve1 input
  print $ solve2 input
