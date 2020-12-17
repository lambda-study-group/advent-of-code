module Main where

import Data.Set (Set(..), member)
import qualified Data.Set as S

data Cell    = Inactive | Active deriving (Eq, Show)
type Coord   = (Int, Int, Int)
type Coord4D = (Int, Int, Int, Int)

-- * Parsing

parse :: (Int -> String -> [a]) -> String -> [a]
parse pLine input = concat
                  $ zipWith pLine [0..]
                  $ lines input

parseLine :: Int  -> String -> [Coord]
parseLine x css = foldr getActive []
                $ zip [0..]
                $ map parseChar css
  where
    getActive (_, Inactive) xs = xs
    getActive (y, Active)   xs = (x,y,0):xs


parseLine4D :: Int  -> String -> [Coord4D]
parseLine4D x css = foldr getActive []
                  $ zip [0..]
                  $ map parseChar css
  where
    getActive (_, Inactive) xs = xs
    getActive (y, Active)   xs = (x,y,0,0):xs

parseChar :: Char -> Cell
parseChar '.' = Inactive
parseChar '#' = Active
parseChar  _  = error "invalid char"

-- * Update Automata
update :: (Ord a) => (a -> [a]) -> Set a -> Set a
update neighFun s = S.fromList $ coordsActives ++ coordsInactives
  where
    coords          = S.toList s
    coordsActives   = filter ((`elem`[2,3]).getActiveCoords) coords
    coordsInactives = concatMap getInactiveCoords coords

    getActiveCoords   coord = let activeNeigh = filter (`member` s) $ neighFun coord
                              in  length activeNeigh

    getInactiveCoords coord = let inactiveNeigh = filter (`notMember` s) $ neighFun coord
                              in  filter ((==3).getActiveCoords) inactiveNeigh

notMember :: Ord a => a -> Set a -> Bool
notMember x s = not (member x s)

neighbors :: Coord -> [Coord]
neighbors (x, y, z) = sum3D (x,y,z) <$> filter (/=(0,0,0)) coords
  where
    bounds = [-1..1]
    coords = (,,) <$> bounds <*> bounds <*> bounds
    sum3D (x,y,z) (a,b,c) = (x+a, y+b, z+c)

neighbors4D :: Coord4D -> [Coord4D]
neighbors4D (x, y, z, w) = sum4D (x,y,z,w) <$> filter (/=(0,0,0,0)) coords
  where
    bounds = [-1..1]
    coords = (,,,) <$> bounds <*> bounds <*> bounds <*> bounds
    sum4D (x,y,z,w) (a,b,c,d) = (x+a, y+b, z+c, w+d)

example = ".#.\n..#\n###"

main :: IO ()
main = do
  dat <- readFile "day17.txt"
  let s0     = S.fromList $ parse parseLine dat
      s04D   = S.fromList $ parse parseLine4D dat
      sets   = iterate (update neighbors) s0
      sets4D = iterate (update neighbors4D) s04D
  print $ S.size (sets !! 6)
  print $ S.size (sets4D !! 6)
