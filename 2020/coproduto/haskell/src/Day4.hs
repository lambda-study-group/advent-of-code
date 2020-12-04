{-# LANGUAGE OverloadedStrings #-}

module Day4
  ( part1
  , part2
  ) where

import qualified Debug.Trace
import qualified Data.Char as Char
import Data.List (unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Set (Set, isSubsetOf)
import qualified Data.Set as Set
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Common (readFileUtf8)

part1 :: IO ()
part1 = countPassingPassports passportHasRequiredFields

part2 :: IO ()
part2 = countPassingPassports (\p -> passportHasRequiredFields p && passportIsValid p)

countPassingPassports :: (Map Text Text -> Bool) -> IO ()
countPassingPassports isPassing = do
  passports <- parsePassports <$> readFileUtf8 "input/day_4.txt"
  print (count isPassing passports)

passportHasRequiredFields :: Map Text Text -> Bool
passportHasRequiredFields passport = requiredFields `isSubsetOf` (Set.fromList $ Map.keys passport)

eitherToBool :: Either a Bool -> Bool
eitherToBool (Right True) = True
eitherToBool (Right _)    = False
eitherToBool (Left _)     = False

passportIsValid :: Map Text Text -> Bool
passportIsValid passport = eitherToBool $ do
  (byr, _) <- Read.decimal $ passport ! "byr"
  (iyr, _) <- Read.decimal $ passport ! "iyr"
  (eyr, _) <- Read.decimal $ passport ! "eyr"
  let hgt = passport ! "hgt"
  let hcl = passport ! "hcl"
  let ecl = passport ! "ecl"
  let pid = passport ! "pid"
  pure
    $ validateByr byr
    && validateIyr iyr
    && validateEyr eyr
    && validateHgt hgt
    && validateHcl hcl
    && validateEcl ecl
    && validatePid pid

validateByr :: Int -> Bool
validateByr byr = byr >= 1920 && byr <= 2002

validateIyr :: Int -> Bool
validateIyr iyr = iyr >= 2010 && iyr <= 2020

validateEyr :: Int -> Bool
validateEyr eyr = eyr >= 2020 && eyr <= 2030

validateHgt :: Text -> Bool
validateHgt hgt = eitherToBool $ do
  (height, rest) <- Read.decimal hgt
  pure $ (rest == "cm" || rest == "in")
    && (case rest of
          "cm" -> height >= 150 && height <= 193
          "in" -> height >= 59 && height <= 76)

validateHcl :: Text -> Bool
validateHcl hcl =
  case Text.uncons hcl of
    Just ('#', rest) ->
      Text.length rest == 6 && Text.all Char.isHexDigit rest
    _ -> False

validateEcl :: Text -> Bool
validateEcl = flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validatePid :: Text -> Bool
validatePid pid =
  Text.length pid == 9 && Text.all Char.isDigit pid

requiredFields :: Set Text
requiredFields = Set.fromList $
  [ "byr"
  , "iyr"
  , "eyr"
  , "hgt"
  , "hcl"
  , "ecl"
  , "pid"
  ]

parsePassports :: Text -> [Map Text Text]
parsePassports = fmap parsePassport . getPassports

getPassports :: Text -> [Text]
getPassports input =
  filter ((> 0) . Text.length)
  $ Text.unwords
  <$> (split (=="") $ Text.lines input)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

parsePassport :: Text -> Map Text Text
parsePassport input =
  Map.fromList
  $ (mapSnd (Text.drop 1) . (Text.break (==':')))
  <$> Text.words input

count :: (a -> Bool) -> [a] -> Int
count f = length . (filter f)

split :: (a -> Bool) -> [a] -> [[a]]
split f = unfoldr go
   where go [] = Nothing
         go l  =
           let (prefix, rest) = break f l
           in if length rest > 1
              then Just (prefix, tail rest)
              else Just (prefix, rest)
