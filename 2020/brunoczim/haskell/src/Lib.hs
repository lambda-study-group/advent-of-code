module Lib
  ( selectEntries
  , readEntries
  , entriesProd
  ) where

import Data.List (foldl')
import Data.Either (rights)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead

readEntries :: IO [Int]
readEntries = do
  contents <- TIO.getContents
  let lines = Text.lines contents
      report = fmap (fmap fst . TRead.decimal) lines
  pure (rights report)

selectEntries :: Int -> Int -> [Int] -> [Int]
selectEntries _ _ [] = []
selectEntries k 1 (x : xs)
  | x == k = [x]
  | otherwise = selectEntries k 1 xs
selectEntries k n (x : xs) =
  let sub = selectEntries (k - x) (n - 1) xs
  in if k < x || null sub
    then selectEntries k n xs
    else x : sub

entriesProd :: [Int] -> Int
entriesProd = foldl' (*) 1
