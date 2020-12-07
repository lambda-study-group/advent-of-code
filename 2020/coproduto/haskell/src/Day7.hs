{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Debug.Trace
import Data.List (foldl')
import Data.Traversable (for, traverse)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Control.Monad.State

import Common (readFileUtf8)

data BagContents = Empty | Contains [(Int, Text)]
  deriving Show

part1 :: IO ()
part1 = countBags countReachables

part2 :: IO ()
part2 = countBags countContents

countBags :: (Map Text BagContents -> Int) -> IO()
countBags f = do
  contentMap <- parseFile <$> readFileUtf8 "input/day_7.txt"
  print (f contentMap)

parseFile :: Text -> Map Text BagContents
parseFile input = Map.fromList $ parseLine <$> Text.lines input

countContents :: Map Text BagContents -> Int
countContents contentMap =
  let initialState = initReachabilityMap contentMap
      keys         = Map.keys contentMap
      totalBags    = evalState (countContents' contentMap "shiny gold") initialState
  in totalBags - 1

countContents' :: Map Text BagContents -> Text -> State (Map Text (Maybe Int)) Int
countContents' contentsMap key = do
  reachabilityMap <- get
  case reachabilityMap ! key of
    Just amount -> return amount
    Nothing -> case contentsMap ! key of
      Empty -> do
        modify (Map.insert key (Just 1))
        return 1
      Contains amounts -> do
        recursiveAmounts <- for amounts $
          \(amount, color) -> (amount *) <$> countContents' contentsMap color
        let total = 1 + sum' recursiveAmounts
        modify (Map.insert key (Just total))
        return total

countReachables :: Map Text BagContents -> Int
countReachables contentMap =
  let initialState = initReachabilityMap contentMap
      keys         = Map.keys contentMap
      finalState   = execState ((traverse $ search contentMap) keys) initialState
  in length $ filter (==(Just True)) $ Map.elems finalState

search :: Map Text BagContents -> Text -> State (Map Text (Maybe Bool)) Bool
search contentsMap key = do
  reachabilityMap <- get
  case reachabilityMap ! key of
    Just reachable -> return reachable
    Nothing        -> case contentsMap ! key of
      Empty        -> do
        modify (Map.insert key (Just False))
        return False
      Contains set
        | "shiny gold" `elem` (snd <$> set) -> do
            modify (Map.insert key (Just True))
            return True
        | otherwise -> do
            containsRecursively <- or <$> traverse (search contentsMap) (snd <$> set)
            modify (Map.insert key (Just containsRecursively))
            return containsRecursively

initReachabilityMap :: Map Text a -> Map Text (Maybe b)
initReachabilityMap = Map.fromList . (flip zip $ repeat Nothing) . Map.keys

parseLine :: Text -> (Text, BagContents)
parseLine line =
  let (color, rest) = splitAt 2 $ Text.words line
      contentsList  = drop 2 rest
      contents      = parseContentsList contentsList
  in (Text.unwords color, contents)

parseContentsList :: [Text] -> BagContents
parseContentsList list
  | length list == 3 = Empty
  | otherwise =
    let contents = splitAt 1 <$> take 3 <$> chunksOf 4 list
    in Contains $ contents <&> \([amountText], color) ->
      case Read.decimal amountText of
        Right (amount, _) -> (amount, Text.unwords color)
        _                 -> error "Invalid input."

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

chunksOf :: Int -> [a] -> [[a]]
chunksOf n l =
  let (prefix, suffix) = splitAt n l
  in case suffix of
    [] -> [prefix]
    _  -> prefix : (chunksOf n suffix)
