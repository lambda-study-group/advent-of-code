module Main where

import Data.Char      (digitToInt)
import Numeric        (readInt)
import Data.List      (sort)

getId :: String -> Int
getId = fst . head . readInt 2 (`elem` "01") digitToInt . map parser
  where
    parser 'F' = '0'
    parser 'B' = '1'
    parser 'R' = '1'
    parser 'L' = '0'

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
  let seats = map getId contents
  print $ maximum seats
  print $ findId seats
