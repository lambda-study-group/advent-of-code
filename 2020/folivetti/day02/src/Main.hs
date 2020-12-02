module Main where

import Data.List.Split
import qualified Data.Map.Strict as M

type Validator = (Int, Int, Char, String)

-- | We can assume the data is well formed, so we don't need anything too sophisticated
parse :: String -> Validator 
parse input = 
  case words input of
       [rng, c, password] -> let (lo:hi:_) = splitOn "-" rng
                             in  (read lo, read hi, head c, password)
       _                  -> error ("Parser error with " ++ input)

-- Part 1
isValidPass1 :: Validator -> Bool
isValidPass1 (l, h, c, p) = 
  let counter = M.fromListWith (+) $ zip p (repeat 1)
  in  case counter M.!? c of
           Nothing -> l == 0
           Just n  -> n >= l && n <= h 

-- Part 2
isValidPass2 :: Validator -> Bool
isValidPass2 (l, h, c, p) = (p !* l == c) /= (p !* h == c)
  where
    -- same as !! but adjusting the index to 1-based
    -- there's not enough operators in Haskell!
    xs !* i = xs !! (i-1)

main :: IO ()
main = do
  inputs <- map parse . lines <$> readFile "day02.txt"
  let howMany f = length . filter id . map f
  print $ howMany isValidPass1 inputs
  print $ howMany isValidPass2 inputs
