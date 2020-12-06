module Day5 where

import Data.List (sort)
import Data.Bifunctor (bimap)


{-@ type NonEmpty a = {v:[a] | len v > 0} @-}


{-@ decodeRow :: (Nat, Nat) -> String -> Nat @-}
decodeRow :: (Int, Int) -> String -> Int
decodeRow (lo, hi) [] = (hi + lo) `div` 2
decodeRow range (x:xs) =
  let range' = decodeRowLetter range x in
    decodeRow range' xs

{-@ decodeRowLetter :: (Nat, Nat) -> Char -> (Nat, Nat) @-}
decodeRowLetter :: (Int, Int) -> Char -> (Int, Int)
decodeRowLetter (lo, hi) 'F' = let mid = (hi + lo) `div` 2 in (lo, mid)
decodeRowLetter (lo, hi) 'B' = let mid = (hi + lo) `div` 2 in (mid + 1, hi)
decodeRowLetter (lo, hi) _   = (lo, hi)

{-@ decodeCol :: (Nat, Nat) -> String -> Nat @-}
decodeCol :: (Int, Int) -> String -> Int
decodeCol (lo, hi) [] = (hi + lo) `div` 2
decodeCol range (x:xs) =
  let range' = decodeColLetter range x in
    decodeCol range' xs

{-@ decodeColLetter :: (Nat, Nat) -> Char -> (Nat, Nat) @-}
decodeColLetter :: (Int, Int) -> Char -> (Int, Int)
decodeColLetter (lo, hi) 'L' = let mid = (hi + lo) `div` 2 in (lo, mid)
decodeColLetter (lo, hi) 'R' = let mid = (hi + lo) `div` 2 in (mid + 1, hi)
decodeColLetter (lo, hi) _   = (lo, hi)

{-@ seatId :: (Nat, Nat) -> Nat @-}
seatId :: (Int, Int) -> Int
seatId (row, col) = (8 * row) + col

{-@ decodeTicket :: String -> Nat @-}
decodeTicket :: String -> Int
decodeTicket =
  let
    r1 = (0, 127)
    r2 = (0, 7)
  in
    seatId . bimap (decodeRow r1) (decodeCol r2) . splitAt 7

{-@ solve1 :: NonEmpty String -> Nat @-}
solve1 :: [String] -> Int
solve1 = maximum . fmap decodeTicket


{-@ findSeat :: NonEmpty Nat -> Nat @-}
findSeat :: [Int] -> Int
findSeat [x] = 0
findSeat (y:x:xs) =
  if x == y + 1 then findSeat (x:xs) else y + 1


{-@ assume sort :: v:[a] -> {u:[a] | len v = len u} @-}

{-@ solve2 :: NonEmpty String -> Nat @-}
solve2 :: [String] -> Int
solve2 = findSeat . sort . map decodeTicket


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath

  let tickets = lines input

  if length tickets > 0
    then do
      putStrLn $ "Part 1: " <> (show . solve1) tickets
      putStrLn $ "Part 2: " <> (show . solve2) tickets
    else
      putStrLn "No tickets found"
