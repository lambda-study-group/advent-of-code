module Day2
  ( part1
  , part2
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

import Common (readFileUtf8)

data PasswordVerification = PasswordVerification
  { i1             :: Int
  , i2             :: Int
  , letterToVerify :: Char
  , password       :: Text
  }

part1 :: IO ()
part1 = countPassingCases verifyByCount

part2 :: IO ()
part2 = countPassingCases verifyByPosition

countPassingCases :: (PasswordVerification -> Bool) -> IO ()
countPassingCases verify = do
  input <- parseInput <$> readFileUtf8 "input/day_2.txt"
  case input of
   Nothing -> putStrLn "Invalid input."
   Just cases ->
     print $ length . filter (== True) $ verify <$> cases

parseInput :: Text -> Maybe [PasswordVerification]
parseInput input =
  let lines = Text.lines input
  in case sequence $ parseVerification <$> lines of
    Right entries -> Just entries
    Left _        -> Nothing

parseVerification :: Text -> Either String PasswordVerification
parseVerification input = do
  (in1, rest) <- TextRead.decimal input
  (in2, rest') <- TextRead.decimal (Text.drop 1 rest)
  let char = Text.head (Text.drop 1 rest')
  pure $ PasswordVerification
    { i1             = in1
    , i2             = in2
    , letterToVerify = char
    , password       = Text.drop 4 rest'
    }
  
verifyByCount :: PasswordVerification -> Bool
verifyByCount vcase = letterCount >= minCount && letterCount <= maxCount
  where minCount    = i1 vcase
        maxCount    = i2 vcase
        letterCount = countChar (letterToVerify vcase) (password vcase)
        countChar c = Text.length . Text.filter (== c)

verifyByPosition :: PasswordVerification -> Bool
verifyByPosition vcase = (firstOk && not secondOk) || (not firstOk && secondOk)
  where target         = letterToVerify vcase
        firstChar      = Text.index (password vcase) ((i1 vcase) - 1)
        secondChar     = Text.index (password vcase) ((i2 vcase) - 1)
        firstOk        = firstChar  == target
        secondOk       = secondChar == target
