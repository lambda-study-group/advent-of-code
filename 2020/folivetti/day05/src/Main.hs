module Main where

import Data.Char      (digitToInt)
import Numeric        (readInt)
import Data.List      (sort)
import Data.Bifunctor

parse2bin :: (Char -> Char) -> String -> Int
parse2bin parser = fst . head . readInt 2 (`elem` "01") digitToInt . map parser

parserFB :: Char -> Char
parserFB 'F' = '0'
parserFB 'B' = '1'
parserFB  _  = error "Invalid character in parserFB"

parserRL :: Char -> Char
parserRL 'R' = '1'
parserRL 'L' = '0'
parserRL  _  = error "Invalid character in parserRL"

parseSeat :: String -> (Int, Int)
parseSeat = bimap fb rl . splitAt 7
  where
    fb = parse2bin parserFB
    rl = parse2bin parserRL

getId :: (Int, Int) -> Int
getId (x,y) = x * 8 + y

findId :: [Int] -> Maybe Int
findId = go . sort
  where
    go []         = Nothing 
    go (x:y:xs)
      | y == x+2  = Just $ x+1
      | otherwise = go (y:xs)

main :: IO ()
main = do
  contents <- lines <$> readFile "day05.txt"
  let seats = map (getId.parseSeat) contents
  print $ maximum seats
  print $ findId seats
