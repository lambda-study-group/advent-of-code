module Main where

import Data.List (foldl')
import Data.Either (either)
import Text.Parsec
import qualified Data.Map.Strict as M

-- | Parsing data
getMap :: Parsec String () (M.Map String [(String, Int)])
getMap = M.fromList <$> getAllLines

getAllLines :: Parsec String () [(String, [(String, Int)])]
getAllLines = many getSingleLine

getSingleLine :: Parsec String () (String, [(String, Int)])
getSingleLine = do
  key <- getBagColor
  string " contain "
  vals <- getValuesIf
  many (char '\n')
  return (key, vals)

getValuesIf :: Parsec String () [(String, Int)]
getValuesIf = (string "no other bags." >> return []) <|> getValues

getValues :: Parsec String () [(String, Int)]
getValues = do
  n    <- getQty
  char ' ' 
  bag  <- getBagColor
  bags <- (string ", " >> getValues) <|> (char '.' >> return [])
  return $ (bag, n):bags

getBagColor :: Parsec String () String
getBagColor = do
  c1 <- manyTill letter (char ' ')
  c2 <- manyTill letter (char ' ')
  string "bag" 
  many $ string "s"
  return (c1 ++ c2)

getQty :: Parsec String () Int
getQty = read . (:"") <$> digit

-- Part 1: the DAG cannot have any cycles or we would have
-- an infinity amount of bags. So the recursion will eventually end.
part1 :: M.Map String [(String, Int)] -> Int
part1 bags = M.size $ M.filter (any hasGold) bags
  where
    hasGold ("shinygold", _) = True
    hasGold (k, _)           = maybe False (any hasGold) $ bags M.!? k

-- Part 2: check the contents of bag k, add the number n of each bag
-- and n times the number of bags it contains.
part2 :: M.Map String [(String, Int)] -> String -> Int
part2 bags k = foldl' add 0 $ bags M.! k
  where
    add acc (k',n) = n + (n * part2 bags k') + acc

main :: IO ()
main = do
  bags <- either (const M.empty) id . parse getMap "" <$> readFile "day07.txt"
  print $ part1 bags
  print $ part2 bags "shinygold"
