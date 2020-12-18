module Main where

import Data.Set (Set(..), member)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Coord = (Int, Int, Int, Int)

-- * Parsing
parse :: String -> Set Coord
parse = go 0 0 S.empty
  where
    go x y s ('#' :cs) = go (x+1) y     (S.insert (x,y,0,0) s) cs
    go x y s ('.' :cs) = go (x+1) y     s                      cs
    go _ y s ('\n':cs) = go 0     (y+1) s                      cs
    go _ _ s []        = s

-- * Update Automata
update :: [Coord] -> Set Coord -> Set Coord
update ds s = S.union s1 s2
  where
    ns          = neighbors ds s
    isActive xs = M.keysSet . M.filter (`elem` xs)
    -- active neighborhood
    s1 = isActive [2,3] $ M.restrictKeys ns s
    -- inactive neighborhood
    s2 = isActive [3]   $ M.withoutKeys ns s

-- returns a map counting the number of active neighbors for each
-- active coordinate and their neighbors
neighbors :: [Coord] -> Set Coord -> M.Map Coord Int
neighbors ds s = M.unionsWith (+) 
               $ map (forKeys m . sumCoord) ds -- each map has a coord as key and the value is the number of active neighbors
  where
    forKeys = flip M.mapKeys        -- for each key (coord) from a map, apply a function
    m       = M.fromSet (const 1) s -- every active cell has value 1

sumCoord :: Coord -> Coord -> Coord
sumCoord (x,y,z,w) (a,b,c,d) = (x+a,y+b,z+c,w+d)

deltas :: [Coord]
deltas = filter (/=(0,0,0,0))
       $ (,,,) <$> [-1..1] <*> [-1..1] <*> [-1..1] <*> [0]

deltas4D :: [Coord]
deltas4D = filter (/=(0,0,0,0))
         $ (,,,) <$> [-1..1] <*> [-1..1] <*> [-1..1] <*> [-1..1]

main :: IO ()
main = do
  dat <- readFile "day17.txt"
  let s0     = parse dat
      sets   = iterate (update deltas) s0
      sets4D = iterate (update deltas4D) s0
  print $ S.size (sets !! 6)
  print $ S.size (sets4D !! 6)
