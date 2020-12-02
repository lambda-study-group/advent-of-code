module Day2 where

import Data.Char (isSpace, isDigit)


type Parser = ([String], String) -> ([String], String)

data Rule = Rule
  { min :: Int
  , max :: Int
  , letter :: Char
  , password :: String
  }

{-@ (<.>) :: v:[a] -> a -> {u:[a] | len u == len v + 1} @-}
(<.>) :: [a] -> a -> [a]
xs <.> x = xs ++ [x]

{-@ at :: v:[a] -> {n:Int | n > 0 && n <= len v} -> a @-}
at :: [a] -> Int -> a
at xs n = xs !! (n - 1)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

{-@ count :: a -> [a] -> Nat @-}
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

maybeArg :: (a -> Bool) -> (Maybe a -> Bool)
maybeArg f = \mx -> case mx of
  Just x -> f x
  Nothing -> False


tokenP :: Char -> Parser
tokenP token (xs, []) = (xs, [])
tokenP token (xs, c:input) =
  if token == c
     then (xs, input)
     else (xs, c:input)

spaceP :: Parser
spaceP (xs, input) =
  let (_, input') = span isSpace input in
    (xs, input')

charP :: Parser
charP (xs, []) = (xs, [])
charP (xs, s:input) = (xs <.> [s], input)

stringP :: Parser
stringP (xs, input) = (xs <.> input, "")

intP :: Parser
intP (xs, input) =
  let (min, input') = span isDigit input in
    (xs <.> min, input')

fromInput :: String -> ([String], String)
fromInput input = ([], input)


listToRule :: [String] -> Maybe Rule
listToRule [min, max, letter, password] =
  if length letter > 0
     then Just $ Rule (read min) (read max) (head letter) password
     else Nothing
listToRule _ = Nothing

parseRule :: String -> Maybe Rule
parseRule = listToRule
  . fst
  . stringP
  . spaceP
  . tokenP ':'
  . charP
  . spaceP
  . intP
  . tokenP '-'
  . intP
  . fromInput

validateRule1 :: Rule -> Bool
validateRule1 (Rule min max letter password) =
  let n = count letter password in
    min <= n && n <= max

validateRule2 :: Rule -> Bool
validateRule2 (Rule min max letter password) =
  if (0 < min && min <= length password) && (0 < max && max <= length password)
    then (password `at` min == letter) `xor` (password `at` max == letter)
    else False

countValid :: (Rule -> Bool) -> [String] -> Int
countValid validation = count True . fmap (maybeArg validation . parseRule)


main :: IO ()
main = do
  putStrLn "Input file: "
  filepath <- getLine
  input <- readFile filepath
  let passwords = lines input
  putStrLn $ "Part 1: " <> show (countValid validateRule1 passwords)
  putStrLn $ "Part 2: " <> show (countValid validateRule2 passwords)
