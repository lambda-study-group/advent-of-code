{-# LANGUAGE OverloadedStrings #-}
module Day14
  ( part1
  , part2
  ) where

import Data.Int (Int64)
import Data.Bits ((.&.), (.|.), testBit, clearBit, setBit, complement, zeroBits)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State
import Common (textToInt, readFileUtf8, sum')

data Inst = SetMask (Int64, Int64, Int64) | SetMem (Int, Int64) deriving Show
type ProgramState = (Map Int Int64, (Int64, Int64, Int64))

run :: (ProgramState -> Inst -> ProgramState) -> IO ()
run execInst = do
  input <- fmap parseLine . Text.lines <$> (readFileUtf8 "input/day_14.txt")
  let (finalMem, _) = foldl' execInst (Map.empty, (0, 0, 0)) input
  print (sum' $ Map.elems finalMem)

part1 :: IO ()
part1 = run runInst

part2 :: IO ()
part2 = run runInstWithFloat

runInst :: ProgramState -> Inst -> ProgramState
runInst (mem, mask) (SetMask newMask)      = (mem, newMask)
runInst (mem, mask) (SetMem (addr, val)) = (Map.insert addr maskedVal mem, mask)
  where maskedVal = applyMask mask val

runInstWithFloat :: ProgramState -> Inst -> ProgramState
runInstWithFloat (mem, mask) (SetMask newMask)      = (mem, newMask)
runInstWithFloat (mem, mask) (SetMem (addr, val)) =
  (foldl' (\mem' addr -> Map.insert addr val mem') mem (fromIntegral <$> maskedAddrs), mask)
  where maskedAddrs = applyMaskWithFloat mask (fromIntegral addr)

binTextToInt64 :: Text -> Int64
binTextToInt64 txt = binTextToInt64' 0 (Text.unpack txt)
  where binTextToInt64' acc []       = acc
        binTextToInt64' acc ('0':cs) = binTextToInt64' (2*acc) cs
        binTextToInt64' acc ('1':cs) = binTextToInt64' (2*acc + 1) cs

parseMask :: Text -> (Int64 ,Int64, Int64)
parseMask mask = (floatMask, andMask, orMask)
  where andMask   = binTextToInt64 (Text.replace "X" "1" mask)
        orMask    = binTextToInt64 (Text.replace "X" "0" mask)
        floatMask =
          binTextToInt64 (Text.replace "X" "1" (Text.replace "1" "0" mask))

parseSet :: Text -> (Int, Int64)
parseSet text = (addr, arg)
  where (_, rest)         = Text.break (=='[') text
        (addrText, rest') = Text.break (==']') rest
        arg               = textToInt (last (Text.words rest'))
        addr              = textToInt (Text.drop 1 addrText)

applyMask :: (Int64, Int64, Int64) -> Int64 -> Int64
applyMask (_, andMask, orMask) n = andMask .&. (orMask .|. n)

applyMask' :: (Int64, Int64, Int64) -> Int64 -> Int64
applyMask' (_, andMask, orMask) n = orMask .|. (andMask .&. n)

makeFloatMasks :: Int64 -> [(Int64, Int64, Int64)]
makeFloatMasks mask =
  let bits      = filter (testBit mask) [0..35]
      subMasks  = powerset bits
      setBits   = foldl' setBit  0
      unsetAll  = foldl' clearBit (complement zeroBits) bits
  in (\mask -> (0, unsetAll, setBits mask)) <$> subMasks

powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

applyMaskWithFloat :: (Int64, Int64, Int64) -> Int64 -> [Int64]
applyMaskWithFloat mask n =
  let (floatMask, _, orMask) = mask
      floatMasks             = makeFloatMasks floatMask
  in zipWith applyMask' floatMasks (repeat $ orMask .|. n)

parseLine :: Text -> Inst
parseLine line
  | "mask" `Text.isPrefixOf` line = SetMask $ parseMask (last $ Text.words line)
  | otherwise                     = SetMem $ parseSet line
