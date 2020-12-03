module Day3 where

import Data.Maybe (fromMaybe)


data Place = Empty | Tree deriving (Eq, Show)

type Forest = [[Place]]


toForest :: [[Char]] -> Forest
toForest = fmap (fmap h)
  where h '#' = Tree
        h _   = Empty

(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(x:xs) !? 0 = Just x
(x:xs) !? n
  | n > 0 = xs !? (n - 1)
  | otherwise = Nothing

at :: Int -> Int -> Forest -> Maybe Place
at x y forest = (forest !? x) >>= (\f -> f !? y)

first :: [a] -> Maybe a
first [] = Nothing
first (x:xs) = Just x


{-@ type Pos = {n:Nat | n > 0} @-}

{-@
step :: a:Pos -> Int -> Forest -> v:(Int, Int) -> {u:(Int, Int) | fst u > fst v}
@-}
step :: Int -> Int -> Forest -> (Int, Int) -> (Int, Int)
step a b forest (x, y) =
  if line > 0
     then (x + a, (y + b) `mod` line)
     else (x + a, y + b)
  where line = (fromMaybe 0 . fmap length . first) forest


{-@
getTrees :: [Place] -> Pos -> Int -> h:Int -> Forest -> x:Int -> Int -> [Place] / [h - x]
@-}
getTrees :: [Place] -> Int -> Int -> Int -> Forest -> Int -> Int -> [Place]
getTrees path a b height forest x y =
  let
    (x', y') = step a b forest (x, y)
    place = fromMaybe Empty (at x' y' forest)
  in
    if x' < height
       then getTrees (place:path) a b height forest x' y'
       else path

{-@ countTrees :: v:[Place] -> {n:Nat | n <= len v} @-}
countTrees :: [Place] -> Int
countTrees [] = 0
countTrees (x:xs)
  | x == Tree = 1 + countTrees xs
  | otherwise = countTrees xs


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath
  let forest = (toForest . lines) input
  let height = length forest

  putStrLn $
    "Part 1: " <> show (countTrees (getTrees [] 1 3 height forest 0 0))

  let path1 = countTrees (getTrees [] 1 1 height forest 0 0)
  let path2 = countTrees (getTrees [] 1 3 height forest 0 0)
  let path3 = countTrees (getTrees [] 1 5 height forest 0 0)
  let path4 = countTrees (getTrees [] 1 7 height forest 0 0)
  let path5 = countTrees (getTrees [] 2 1 height forest 0 0)

  let result = product [path1, path2, path3, path4, path5]

  putStrLn $ "Part2: " <> show result
