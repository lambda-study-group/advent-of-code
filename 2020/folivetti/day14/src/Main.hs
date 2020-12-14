{-# language TupleSections #-}
module Main where

import Data.List hiding (insert, union)
import Data.List.Split
import Data.Map.Strict (Map(..), insert, empty, fromList, union)
import qualified Data.Map.Strict as M

data Bit = Zero | One | NotCare deriving Show

type Mask        = [Bit]
type Index       = Int
type Value       = Int
type Mem         = Map Int Int
type Instruction = (Mem, Mask) -> String -> (Mem, Mask)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x       = [x]
  | otherwise = x : takeUntil p xs

fillBits :: [Int] -> [Int]
fillBits xs
  | n == 36   = xs
  | otherwise = xs ++ replicate (36-n) 0
  where
    n = length xs

int2Bits :: Int -> [Int]
int2Bits = fillBits . map snd . tail . takeUntil ((==0).fst) . iterate ((`quotRem` 2) . fst) . (,0)

toBit :: Int -> Bit
toBit 0 = Zero
toBit 1 = One
toBit _ = NotCare

bits2Int :: [Int] -> Int
bits2Int xs = go xs 1 0
  where
    go [] _ n = n
    go (0:bs) mul n = go bs (2*mul) n
    go (1:bs) mul n = go bs (2*mul) (n + mul)


combineBits :: Bit -> Int -> Int
combineBits Zero    _ = 0
combineBits One     _ = 1
combineBits NotCare x = x

parseMask :: String -> Mask
parseMask []       = []
parseMask ('0':cs) = Zero    : parseMask cs
parseMask ('1':cs) = One     : parseMask cs
parseMask ('X':cs) = NotCare : parseMask cs

parseInstruction :: String -> Either Mask (Int, Int)
parseInstruction code
  | "mask = " `isPrefixOf` code = Left $ reverse $ parseMask mask
  | otherwise                   = Right (pos, val)
  where
    mask  = splitOn " = " code !! 1
    code' = splitOn "] = " $ drop 4 code
    pos   = read (head code')
    val   = read (code' !! 1)

maskAddresses :: Mask -> [Int] -> [Int]
maskAddresses mask bits = go mask bits 1 [0]
  where
    go _            []     _   xs = xs
    go (Zero:ms)    (0:bs) mul xs = go ms bs (2*mul) xs
    go (Zero:ms)    (1:bs) mul xs = go ms bs (2*mul) (map (+mul) xs)
    go (One :ms)    (_:bs) mul xs = go ms bs (2*mul) (map (+mul) xs)
    go (NotCare:ms) (_:bs) mul xs = go (Zero:ms) (0:bs) mul xs ++ go (Zero:ms) (1:bs) mul xs

applyInstruction :: Instruction
applyInstruction (mem, mask) code =
  case parseInstruction code of
       Left m           -> (mem, m)
       Right (pos, val) -> (insert pos (updateVal val) mem, mask)
  where
    updateVal = bits2Int . zipWith combineBits mask . int2Bits

applyInstructionV2 :: Instruction
applyInstructionV2 (mem, mask) code =
  case parseInstruction code of
       Left m           -> (mem, m)
       Right (pos, val) -> (updateMem val pos `union` mem, mask)
  where
    updateMem val = fromList . map (,val) . maskAddresses mask . int2Bits

runCode :: Instruction -> (Mem, Mask) -> [String] -> Mem
runCode _      mm         []       = fst mm
runCode apply (mem, mask) (op:ops) = let (mem', mask') = apply (mem, mask) op
                                     in  runCode apply (mem', mask') ops


main :: IO ()
main = do
  contents <- lines <$> readFile "day14.txt"
  let mask     = replicate 36 NotCare
      sumTot f = M.foldr (+) 0 $ runCode f (empty, mask) contents
  print $ sumTot applyInstruction 
  print $ sumTot applyInstructionV2 
