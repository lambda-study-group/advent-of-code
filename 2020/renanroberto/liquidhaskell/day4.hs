module Day4 where

import Text.Read (readMaybe)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first, second)
import Control.Monad ((<=<))


type Passport = [(String, String)]


splitBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitBy pred xs =
  let
    ys = takeWhile (not . pred) xs
    zs = dropWhile (not . pred) xs
  in
    if (length zs == 0)
       then ys : []
       else ys : splitBy pred (tail zs)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf _ [] = False
isSubsetOf (x:xs) ys = (x `elem` ys) && (xs `isSubsetOf` ys)

{-@ tailOrEmpty :: v:[a] -> {u:[a] | len u > 0 => len u = len v - 1} @-}
tailOrEmpty :: [a] -> [a]
tailOrEmpty [] = []
tailOrEmpty xs = tail xs

{-@ countElem :: a -> v:[a] -> {n:Nat | n <= len v} @-}
countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (== x)


requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


getPassports :: String -> [String]
getPassports = fmap unwords . splitBy (== "") . lines

naiveValidation :: String -> Bool
naiveValidation =
  (requiredFields `isSubsetOf`) . fmap (fst . break (== ':')) . words

{-@ solve1 :: String -> Nat @-}
solve1 :: String -> Int
solve1 = countElem True . fmap naiveValidation . getPassports


toPassport :: String -> Passport
toPassport = fmap (second tailOrEmpty . break (== ':')) . words

valRange :: Int -> Int -> Int -> Bool
valRange lo hi x = lo <= x && x <= hi

valYer :: String -> Int -> Int -> Passport -> Bool
valYer field lo hi =
  fromMaybe False
  . fmap (valRange lo hi)
  . (readMaybe <=< lookup field)

valByr :: Passport -> Bool
valByr = valYer "byr" 1920 2002

valIyr :: Passport -> Bool
valIyr = valYer "iyr" 2010 2020

valEyr :: Passport -> Bool
valEyr = valYer "eyr" 2020 2030

valHgtM :: Passport -> Maybe Bool
valHgtM passport = do
  field <- lookup "hgt" passport
  let (heightStr, unit) = break (not . isDigit) field
  height <- readMaybe heightStr
  case unit of
    "cm" -> return $ valRange 150 193 height
    "in" -> return $ valRange 59 76 height
    _ -> Nothing

valHgt :: Passport -> Bool
valHgt = fromMaybe False . valHgtM

isHex :: String -> Bool
isHex ('#':str) =
  let
    valLen = length str == 6
    valDig = and (map (\d -> isDigit d || d `elem` ['a' .. 'f']) str)
  in valLen && valDig
isHex _ = False

valHcl :: Passport -> Bool
valHcl = fromMaybe False . fmap isHex . lookup "hcl"

validEyeColor :: [String]
validEyeColor =
  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

valEcl :: Passport -> Bool
valEcl = fromMaybe False
  . fmap (`elem` validEyeColor)
  . lookup "ecl"

valPidM :: Passport -> Maybe Bool
valPidM passport = do
  pid <- lookup "pid" passport
  let valLen = length pid == 9
  let valDig = and (map isDigit pid)
  return (valLen && valDig)

valPid :: Passport -> Bool
valPid = fromMaybe False . valPidM

validations :: [Passport -> Bool]
validations =
  [valByr, valIyr, valEyr, valHgt, valHcl, valEcl, valPid]

completeValidation :: String -> Bool
completeValidation =
  and . (validations <*>) . pure . toPassport
    
{-@ solve2 :: String -> Nat @-}
solve2 :: String -> Int
solve2 = countElem True . fmap completeValidation . getPassports


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath
  putStrLn $ "Part 1: " <> (show . solve1) input
  putStrLn $ "Part 2: " <> (show . solve2) input
