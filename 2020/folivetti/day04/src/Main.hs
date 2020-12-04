module Main where

import Data.List
import Text.Read

groupByEmpty :: [[String]] -> [[String]]
groupByEmpty []  = []
groupByEmpty css = concat cur : groupByEmpty next'
  where
     (cur, next) = break null css
     next' = if null next then next else tail next

containsKey :: String -> [String] -> Bool
containsKey key = any (key `isPrefixOf`)

isAlmostValid :: [String] -> Bool
isAlmostValid dat = all (`containsKey` dat) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

key,value :: String -> String
key   = take 3
value = drop 4

validateRange :: Read a => (a -> Bool) -> String -> Bool
validateRange p val = maybe False p $ readMaybe val

within :: Int -> Int -> Int -> Bool
within lo hi x = x >= lo && x <= hi 

validateBYR = validateRange (within 1920 2002)
validateIYR = validateRange (within 2010 2020)
validateEYR = validateRange (within 2020 2030)

validateHGT [d1,d2,d3,'c','m'] = validateRange (within 150 193) [d1,d2,d3]
validateHGT [d1,d2,'i','n']    = validateRange (within 59 76) [d1,d2]
validateHGT _ = False

validateHCL ('#':vls) = length vls == 6 && all (`elem` "0123456789abcdef") vls
validateHCL _ = False

validateECL = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

validatePID val = let parsed = readMaybe val :: Maybe Int
                  in  maybe False (const $ length val == 9) parsed


validateValue :: String -> Bool
validateValue css =
  case key css of
       "byr" -> validateBYR $ value css
       "iyr" -> validateIYR $ value css
       "eyr" -> validateEYR $ value css
       "hgt" -> validateHGT $ value css
       "hcl" -> validateHCL $ value css
       "ecl" -> validateECL $ value css
       "pid" -> validatePID $ value css
       _     -> True

isFullyValid :: [String] -> Bool
isFullyValid pass = isAlmostValid pass && all validateValue pass

main :: IO ()
main = do
  contents <- groupByEmpty . map words . lines <$> readFile "day04.txt"
  print $ length $ filter isAlmostValid contents
  print $ length $ filter isFullyValid contents
