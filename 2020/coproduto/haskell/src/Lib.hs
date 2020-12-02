{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( day1Part1,
      day1Part2
    ) where

import Data.Maybe (isJust)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Control.Monad (join)
import System.IO (withFile, hSetEncoding, utf8, IOMode(ReadMode))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import Data.Vector (Vector, (//), (!), (!?))
import qualified Data.Vector as Vector

day1Part1 :: IO ()
day1Part1 = do
  input <- parseInput <$> readFileUtf8 "input/day_1.txt"
  case findElementPair 2020 (fillPigeonholes input) of
    Just (a, b) -> print (a * b)
    Nothing     -> print "No matching pair."

day1Part2 :: IO ()
day1Part2 = do
  input <- parseInput <$> readFileUtf8 "input/day_1.txt"
  case findElementTriple 2020 (fillPigeonholes input) of
    Just (a, b, c) -> print (a * b * c)
    Nothing        -> print "No matching pair."
  

parseInput :: Text -> [Int]
parseInput input = fst
  <$> fromRight (0, "")
  <$> TextRead.decimal
  <$> Text.lines input

fillPigeonholes :: [Int] -> Vector (Maybe ())
fillPigeonholes values = Vector.replicate 2020 Nothing // indexPairs
  where indexPairs = zip values $ repeat (Just ())

findElementPair :: Int -> Vector (Maybe ()) -> Maybe (Int, Int)
findElementPair target vec = (,) <$> n <*> ((target -) <$> n)
  where validIndices = Vector.elemIndices (Just ()) vec
        n            = Vector.find (sumsTo vec target) validIndices

sumsTo :: Vector (Maybe ()) -> Int -> Int -> Bool
sumsTo vec n x = isJust $ join (vec !? (n - x))

findElementTriple :: Int -> Vector (Maybe ()) -> Maybe (Int, Int, Int)
findElementTriple target vec = join $ Vector.find isJust mapped
  where validIndices = Vector.elemIndices (Just ()) vec
        mapped       = validIndices <&> \first ->
          case findElementPair (target - (3 * first + 2)) (Vector.drop (first + 1) vec) of
            Just (second, third) -> Just (first, second +first + 1, third + first + 1)
            Nothing              -> Nothing

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = withFile fp ReadMode $ \h -> do
  hSetEncoding h utf8
  TextIO.hGetContents h
