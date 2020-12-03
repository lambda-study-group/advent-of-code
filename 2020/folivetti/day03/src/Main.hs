module Main where

import Data.Array

data Cell = Tree | Empty 
          deriving (Show, Eq)

type Coord = (Int, Int)
type Grid  = Array Coord Cell

-- Zipper of a 2D-grid
data Forest = Forest Coord Grid

createForest :: String -> Forest
createForest = cell2Forest . str2Cell

cell2Forest :: [[Cell]] -> Forest
cell2Forest cells = Forest (0,0) 
                  $ listArray ((0,0),(m,n)) 
                  $ concat cells
  where
    m = length cells - 1
    n = length (head cells) - 1

str2Cell :: String -> [[Cell]]
str2Cell content = map (map readCell) 
                 $ lines content
  where
    readCell '#' = Tree
    readCell '.' = Empty

-- Go to a direction
go :: (Int -> Int, Int -> Int) -> Forest -> Forest
go (fx, fy) (Forest (x,y) grid) = Forest (newCoord grid) grid
  where
    newCoord = loopThru (fx x, fy y) . snd . bounds 

-- If it reaches the extreme bottom or right, goes back
loopThru :: Coord -> Coord -> Coord
loopThru (x,y) (hx,hy) = (x', y')
  where
    x' = if x > hx then 0 else x
    y' = if y > hy then 0 else y

right, down :: (Int -> Int, Int -> Int)
right = ((+0), (+1))
down  = ((+1), (+0))

goRight, goDown :: Forest -> Forest
goRight = go right
goDown  = go down

chainOf :: Int -> (a -> a) -> a -> a
chainOf n f = foldr (.) id $ replicate n f

-- ruleXY = Right X, Down Y
rule11, rule12, rule31, rule51, rule71 :: Forest -> Forest
rule31 = goDown . chainOf 3 goRight
rule11 = goDown . goRight
rule51 = goDown . chainOf 5 goRight
rule71 = goDown . chainOf 7 goRight
rule12 = chainOf 2 goDown . goRight

-- Walk to bottom
walkToBottom :: (Forest -> Forest) -> Forest -> [Forest]
walkToBottom rule = takeUntil isBottom . iterate rule

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x       = [x]
  | otherwise = x : takeUntil p xs

isBottom :: Forest -> Bool
isBottom (Forest (x,_) grid) = x == x'
  where
       ((_,_),(x',_)) = bounds grid

get :: Forest -> Cell
get (Forest coord grid) = grid ! coord

countTrees :: (Forest -> Forest) -> Forest -> Int
countTrees rule = length . filter (==Tree) . map get . walkToBottom rule

main :: IO ()
main = do
  content <- readFile "day03.txt"
  let grid = createForest content
  print $ countTrees rule31 grid 
  print $ product $ map (`countTrees` grid) [rule11,rule12,rule31,rule51,rule71]
