module Main where

import Data.Either (rights)
import Data.List (find)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TRead

findEntries :: Int -> [Int] -> Maybe (Int, Int)
findEntries _ [] = Nothing
findEntries n (x : xs) = case find ((n ==) . (x +)) xs of
  Just y -> Just (x, y)
  Nothing -> findEntries n xs

readEntries :: IO [Int]
readEntries = do
  contents <- TIO.getContents
  let lines = Text.lines contents
      report = fmap (fmap fst . TRead.decimal) lines
  pure (rights report)

main :: IO ()
main = do
  entries <- readEntries
  let prod = case findEntries 2020 entries of
        Just (x, y) -> x * y
        Nothing -> 0
  print prod 
