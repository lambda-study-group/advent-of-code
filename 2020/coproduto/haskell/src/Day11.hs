{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Common (readFileUtf8)

import Debug.Trace
import Data.List (find)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

data Grid = Grid
  { gridEntries :: Vector Char
  , gridBounds :: (Int, Int)
  } deriving (Eq, Show)

part1 :: IO ()
part1 = do
  grid <- parseGrid <$> readFileUtf8 "input/day_11.txt"
  let neighborhoods = overGrid grid neighborhood
  let updateGrid = updateSeatsByNeighborhood neighborhoods
  print $ countOccupied (untilStable updateGrid grid)
  

part2 :: IO ()
part2 = do
  grid <- parseGrid <$> readFileUtf8 "input/day_11.txt"
  let visibilities = overGrid grid visibles
  let updateGrid = updateSeatsByVisible visibilities
  print $ countOccupied (untilStable updateGrid grid)

countOccupied :: Grid -> Int
countOccupied Grid{gridEntries = e} = length $ (filter (=='#')) $ toList e

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x = untilStable' x (f x)
  where untilStable' prev current
          | prev == current = current
          | otherwise       = untilStable' current (f current)

overGrid :: Grid -> (Grid -> (Int, Int) -> a) -> Vector a
overGrid grid f =
  Vector.fromList $ fmap (f grid) [(x, y) | y <- [0..maxY], x <- [0..maxX]]
  where (maxX, maxY) = gridBounds grid

visibles :: Grid -> (Int, Int) -> [[Int]]
visibles grid pos = 
  [ visibleInDirection grid (-1, -1) pos
  , visibleInDirection grid (0, -1) pos 
  , visibleInDirection grid (1, -1) pos
  , visibleInDirection grid (-1, 0) pos
  , visibleInDirection grid (1, 0) pos
  , visibleInDirection grid (-1, 1) pos
  , visibleInDirection grid (0, 1) pos
  , visibleInDirection grid (1, 1) pos
  ]

visibleInDirection :: Grid -> (Int, Int) -> (Int, Int) -> [Int]
visibleInDirection grid@Grid{gridBounds = (maxX, maxY)} (dx, dy) (x, y)
  | (nextX > maxX || nextX < 0) = []
  | (nextY > maxY || nextY < 0) = []
  | otherwise                   = (positionToIndex grid (nextX, nextY)) : rest
  where nextX = x + dx
        nextY = y + dy
        rest  = visibleInDirection grid (dx, dy) (nextX, nextY)    

updateSeatsByVisible :: Vector [[Int]] -> Grid -> Grid
updateSeatsByVisible vs Grid{gridEntries = e, gridBounds = b} =
  Grid { gridEntries = v, gridBounds = b }
  where v = Vector.generate (length e) $ \i ->
          let visibles      = (fmap . fmap) (e !)(vs ! i)
              firstVisibles = find (\s -> s == '#' || s == 'L') <$> visibles
              occupiedCount = length $ (filter (== Just '#')) firstVisibles
          in updateSeat 5 (e ! i) occupiedCount

updateSeatsByNeighborhood ::Vector [Int] -> Grid -> Grid
updateSeatsByNeighborhood ns Grid{gridEntries = e, gridBounds = b} =
  Grid { gridEntries = v, gridBounds = b }
  where v = Vector.generate (length e) $ \i ->
          let
            neighbors = fmap (e !) (ns ! i)
            neighborCount = length $ filter (=='#') (fmap (e !) (ns ! i))
          in updateSeat 4 (e ! i) neighborCount


updateSeat :: Int -> Char -> Int -> Char
updateSeat _         '.'   _     = '.'
updateSeat _         'L'   0     = '#'
updateSeat _         'L'   _     = 'L'
updateSeat threshold '#' count
  | count >= threshold = 'L'
  | otherwise          = '#'

neighborhood :: Grid -> (Int, Int) -> [Int]
neighborhood g@Grid{gridBounds = (maxX, maxY)} (x, y) = positionToIndex g <$>
  [ (x', y')
  | x' <- [x - 1, x, x + 1]
  , y' <- [y - 1, y, y + 1]
  , x' >= 0, y' >= 0
  , x' <= maxX, y' <= maxY
  , x' /= x || y' /= y
  ]

getSeat :: Grid -> (Int, Int) -> Char
getSeat grid pos = (gridEntries grid) ! positionToIndex grid pos 

positionToIndex :: Grid -> (Int, Int) -> Int
positionToIndex Grid{gridBounds = (maxX, _)} (x, y) = y * (maxX + 1) + x

parseGrid :: Text -> Grid
parseGrid input = Grid
     { gridEntries = Vector.fromList (Text.unpack $ Text.replace "\n" "" input)
     , gridBounds  = (Text.length (head lines) - 1, length lines - 1)
     }
  where lines   = Text.lines input
     
