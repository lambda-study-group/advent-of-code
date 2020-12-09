{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import Debug.Trace
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Common (readFileUtf8)

part1 :: IO ()
part1 = do
  instructions <- parseFile <$> readFileUtf8 "input/day_8.txt"
  print (getAccumulatorValue instructions)

part2 :: IO ()
part2 = do
  instructions <- parseFile <$> readFileUtf8 "input/day_8.txt"
  print (fixAndGetAccum instructions)

getAccumulatorValue :: [(Text, Int)] -> Int
getAccumulatorValue instructions =
  snd $ evalState (runChecking size instMap) (0, 0, Set.empty)
  where instMap = Map.fromList (zip [0..] instructions)
        size    = Map.size instMap

fixAndGetAccum :: [(Text, Int)] -> Int
fixAndGetAccum instructions =
  evalState (runChanging size instMap) (0, 0, Set.empty)
  where instMap = Map.fromList (zip [0..] instructions)
        size    = Map.size instMap

runChanging :: Int -> Map Int (Text, Int) -> State (Int, Int, Set Int) Int
runChanging size instructions = do
  (ip, acc, insts) <- get
  let currentInstruction = instructions ! ip
  case currentInstruction of
    ("acc", _) -> do
      runInstruction currentInstruction
      runChanging size instructions
    ("nop", n) ->
      let changed     = Map.insert ip ("jmp", n) instructions
          (ip', acc') = evalState (runChecking size changed) (ip, acc, insts)
      in if ip' >= size
         then return acc'
         else do
           runInstruction currentInstruction
           runChanging size instructions
    ("jmp", n) ->
      let changed     = Map.insert ip ("nop", n) instructions
          (ip', acc') = evalState (runChecking size changed) (ip, acc, insts)
      in if ip' >= size
         then return acc'
         else do
           runInstruction currentInstruction
           runChanging size instructions

runChecking :: Int -> Map Int (Text, Int) -> State (Int, Int, Set Int) (Int, Int)
runChecking size instructions = do
  (ip, _, _) <- get
  runInstruction (instructions ! ip)
  (ip, acc, insts) <- get
  if (ip `Set.member` insts) || (ip >= size)
    then return (ip, acc)
    else runChecking size instructions
  
runInstruction :: (Text, Int) -> State (Int, Int, Set Int) ()
runInstruction ("nop", _) =
  modify $ \(ip, acc, insts) -> (ip + 1, acc, Set.insert ip insts)
runInstruction ("acc", n) =
  modify $ \(ip, acc, insts) -> (ip + 1, acc + n, Set.insert ip insts)
runInstruction ("jmp", n) =
  modify $ \(ip, acc, insts) -> (ip + n, acc, Set.insert ip insts)

parseFile :: Text -> [(Text, Int)]
parseFile = fmap parsePair . (fmap $ Text.break Char.isSpace) . Text.lines

parsePair :: (Text, Text) -> (Text, Int)
parsePair (i, n) = (i, intN)
  where Right (intN, _) = Read.signed Read.decimal (Text.drop 1 n)
