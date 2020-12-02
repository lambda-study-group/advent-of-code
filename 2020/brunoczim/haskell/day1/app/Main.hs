module Main where

import Lib (readEntries, selectEntries, entriesProd)

main :: IO ()
main = do
  entries <- readEntries
  let selected = selectEntries 2020 3 entries
      prod = entriesProd selected
  print selected
  print prod 
