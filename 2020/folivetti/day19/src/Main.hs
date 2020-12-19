module Main where

import Data.Map.Strict (Map(..),fromList, (!), insert)
import Text.Parsec

data Rule = Single Int | And Rule Rule | Or Rule Rule | Token Char
          deriving Show

-- * Parsing stuff
getToken :: Parsec String () Rule
getToken = do
  char '\"'
  val <- letter
  char '\"'
  return (Token val)

getSingle :: Parsec String () Rule
getSingle = Single . read <$> many digit

getAnd :: Parsec String () Rule
getAnd = do
  rule1 <- getSingle
  char ' '
  And rule1 <$> getSingle

getOr :: Parsec String () Rule
getOr = try (Or <$> getSingle <*> (string " | " >> getSingle)) <|> (Or <$> getAnd <*> (string " | " >> getAnd))


getData :: Parsec String () (Int, Rule)
getData = do
  key  <- read <$> many digit
  string ": "
  rule <- try getToken <|> try getOr <|> try getAnd <|> getSingle
  return (key, rule)

parseMap :: [String] -> Map Int Rule
parseMap css = fromList $ map (fromEither . parse getData "") css

fromEither :: Show a => Either a b -> b
fromEither (Right x) = x
fromEither (Left  x) = error $ show x

-- * Non-deterministic automata
runNDA :: Map Int Rule -> Int -> String -> Bool
runNDA rules k = elem "" . go (Single k)
  where
    go _           ""      = []
    go (Token c)   (c':cs) = [cs | c==c']
    go (Single x)  cs      = go (rules ! x) cs
    go (And r1 r2) cs      = go r1 cs >>= go r2
    go (Or  r1 r2) cs      = go r1 cs ++ go r2 cs

r8  = Or (Single 42) (And (Single 42) (Single 8))
r11 = Or (And (Single 42) (Single 31)) (And (And (Single 42) (Single 11)) (Single 31))

main :: IO ()
main = do
  dat <- lines <$> readFile "day19.txt"
  let rulesStr = takeWhile (/="") dat
      strings  = tail $ dropWhile (/="") dat
      rules    = parseMap rulesStr
      part1    = map (runNDA rules 0) strings
      rules'   = insert 8 r8 $ insert 11 r11 rules
      part2    = map (runNDA rules' 0) strings 
  print $ length $ filter id part1
  print $ length $ filter id part2
