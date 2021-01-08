module Main where

import Data.List (intersect, isPrefixOf)
import Parse

isValid :: Ranges -> Int -> Bool
isValid (min1, max1, min2, max2) x = (x >= min1 && x <= max1) || (x >= min2 && x <= max2)

isValidValue :: [Ranges] -> Int -> Bool
isValidValue ranges x = any (`isValid` x) ranges

getInvalidValues :: [Ranges] -> [Int] -> [Int]
getInvalidValues ranges = filter (not . isValidValue ranges)

validFields :: [Ranges] -> Int -> [Int]
validFields ranges x = map fst
                     $ filter snd
                     $ zip [0..]
                     $ map (`isValid` x) ranges

singleton :: [Int] -> Bool
singleton [_] = True
singleton  _  = False

-- I could keep track of the singletons I've already checked, but meh!
step :: [[Int]] -> [[Int]]
step xss = do let ys = concat $ filter singleton xss
              xs <- xss
              if singleton xs
                 then return xs
                 else return $ filter (not . (`elem` ys)) xs

solve :: [[Int]] -> [Int]
solve xss
  | all singleton xss = concat xss
  | otherwise         = solve (step xss)

main :: IO ()
main = do
  dat <- lines <$> readFile "day16.txt"
  -- ugly one-liners ahead!
  let (names, constraints, myTicket, nearby) = parseData dat
      part1        = sum $ concatMap (getInvalidValues constraints) nearby
      validTickets = filter (null . getInvalidValues constraints) nearby
      info         = map (map (validFields constraints)) validTickets
      ixs          = solve $ foldl1 (zipWith intersect) info
      idxDepart    = map fst $ filter (("departure" `isPrefixOf`).snd) $ zip [0..] names
      part2        = product $ map ((myTicket !!).snd) $ filter ((`elem` idxDepart).fst) $ zip ixs [0..]
  print part1
  print part2
