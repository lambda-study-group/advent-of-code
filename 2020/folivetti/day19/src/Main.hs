module Main where

import Data.Either     (fromRight)
import Data.Bifunctor  (first)
import Data.Map.Strict (Map(..),fromList, (!), insert)
import Text.Parsec

data Rule = Single Int | And Rule Rule | Or Rule Rule | Token Char
          deriving Show

-- * Parsing stuff
getToken :: Parsec String () Rule
getToken = Token <$> between (char '\"') (char '\"') letter

getSingle :: Parsec String () Rule
getSingle = Single . read <$> many digit

getAnd :: Parsec String () Rule
getAnd = And <$> getSingle <*> (char ' ' >> getSingle)

getOr :: Parsec String () Rule
getOr = try (Or <$> getSingle <*> (string " | " >> getSingle)) 
     <|>    (Or <$> getAnd    <*> (string " | " >> getAnd))

getKey :: Parsec String () Int
getKey = read <$> between (string "") (string ": ") (many digit)

getRule :: Parsec String () Rule
getRule = try getToken <|> try getOr <|> try getAnd <|> getSingle

getData :: Parsec String () (Int, Rule)
getData = (,) <$> getKey <*> getRule

parseMap :: [String] -> Map Int Rule
parseMap = fromList . map (fromRight (0, Single 0) . parse getData "") 

-- * Non-deterministic finite automata
runNDFA :: Map Int Rule -> Int -> String -> Bool
runNDFA rules k = elem "" . go (rules ! k)
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
  let (rules, strings) = first parseMap . span (/="") $ dat
      rules'           = insert 8 r8 . insert 11 r11   $ rules
      countWith r      = length . filter (runNDFA r 0) $ strings
  print $ countWith rules
  print $ countWith rules'
