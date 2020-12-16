module Parse where

import Data.List.Split

type Ranges = (Int, Int, Int, Int)

parseRange :: String -> (Int, Int)
parseRange css = let (n1:n2:_) = splitOn "-" css
                 in (read n1, read n2)

parseRanges :: String -> (String, Ranges)
parseRanges css = let (name:field:_)   = splitOn ": " css
                      (range1:range2:_) = splitOn " or " field
                      (min1, max1)      = parseRange range1
                      (min2, max2)      = parseRange range2
                  in  (name, (min1, max1, min2, max2))

parseTicket :: String -> [Int]
parseTicket = map read . splitOn ","

parseData :: [String] -> ([String], [Ranges], [Int], [[Int]]) 
parseData dat =
  let nameAndranges = map parseRanges $ takeWhile (/="") dat
      constraints   = map snd nameAndranges
      names         = map fst nameAndranges
      myTicket      = parseTicket $ dropWhile (/="your ticket:") dat !! 1
      nearby        = map parseTicket $ tail $ dropWhile (/="nearby tickets:") dat
  in  (names, constraints, myTicket, nearby)
